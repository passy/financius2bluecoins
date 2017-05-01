{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Protolude
import Data.Aeson.Lens
import Control.Lens
import Options.Generic (ParseRecord, getRecord)
import Data.Aeson ((.:), (.:?))
import Database.SQLite.Simple (NamedParam((:=)))

import qualified Data.Aeson as Aeson
import qualified Control.Monad.Logger as L
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

data Args = Args
  { financiusFile :: FilePath
  , bluecoinFile :: FilePath
  , mappingFile :: FilePath
  } deriving (Eq, Show, Generic, ParseRecord)

type AccountMapping = HMS.HashMap Text Integer

data BluecoinTransaction = BluecoinTransaction
  { btxItemId :: RowId
  , btxAmount :: Integer
  , btxNotes :: Text
  , btxAccountId :: Integer
  } deriving (Eq, Show)

data FinanciusAccount = FinanciusAccount
  { faccId :: Text
  , faccCurrencyCode :: Text
  } deriving (Eq, Show)

instance Aeson.FromJSON FinanciusAccount where
  parseJSON = Aeson.withObject "account" $ \o ->
    FinanciusAccount
      <$> o .: "id"
      <*> o .: "currency_code"

fToBAccount :: FinanciusAccount -> AccountMapping -> Maybe Integer
fToBAccount FinanciusAccount{faccId} = HMS.lookup faccId

data FinanciusTransaction = FinanciusTransaction
  { ftxId :: Text
  , ftxAccountFromId :: Maybe Text
  , ftxAccountToId :: Maybe Text
  , ftxNote :: Text
  , ftxAmount :: Integer
  } deriving (Eq, Show)

instance Aeson.FromJSON FinanciusTransaction where
  parseJSON = Aeson.withObject "transaction" $ \o ->
    FinanciusTransaction
        <$> o .: "id"
        <*> o .:? "account_from_id"
        <*> o .:? "account_to_id"
        <*> o .: "note"
        <*> o .: "amount"

data TransactionType = Transfer | Income | Expense
  deriving (Eq, Show)

instance Enum TransactionType where
  fromEnum Transfer = 5
  fromEnum Income = 4
  fromEnum Expense = 3

  toEnum 5 = Transfer
  toEnum 4 = Income
  toEnum 3 = Expense
  toEnum i = error $ "Invalid transaction type " <> show i

newtype RowId = RowId { rowId :: Int64 }
  deriving (Show, Eq)

instance SQL.FromRow RowId where
  fromRow = RowId <$> SQL.field

instance SQL.ToField RowId where
  toField (RowId i) = SQL.toField i

getOrCreate
  :: MonadIO io
  => SQL.Connection
  -> (SQL.Connection -> IO [RowId])
  -> (SQL.Connection -> IO ())
  -> io RowId
getOrCreate conn getFn createFn = do
  res <- liftIO $ getFn conn
  case head res of
    Just a -> return a
    Nothing -> liftIO $ do
      createFn conn
      RowId <$> SQL.lastInsertRowId conn

getOrCreateItem :: MonadIO io => SQL.Connection -> Text -> io RowId
getOrCreateItem conn name = do
  getOrCreate conn
    (\c -> SQL.query c "SELECT itemTableId FROM ITEMTABLE WHERE itemName = ?" (SQL.Only name))
    (\c -> SQL.execute c "INSERT INTO ITEMTABLE (itemName) VALUES (?)" (SQL.Only name))

toFinanciusAccountLookupMap :: V.Vector FinanciusAccount -> HMS.HashMap Text FinanciusAccount
toFinanciusAccountLookupMap =
  HMS.fromList . V.toList . V.map (\acc@FinanciusAccount{..} -> (faccId, acc))

main :: IO ()
main = L.runStderrLoggingT $ do
  $(L.logInfo) "Getting started ..."
  args :: Args <- getRecord "financius2bluecoin"
  financiusJson <- liftIO . readFile $ financiusFile args
  mappingJson <- liftIO . readFile $ mappingFile args
  conn <- liftIO . SQL.open $ bluecoinFile args

  let accountMapping :: Either Text (HMS.HashMap Text Text) =
        case first T.pack . Aeson.eitherDecodeStrict $ TE.encodeUtf8 mappingJson of
          Left e -> error $ "mapping parsing failed: " <> e
          Right a -> a

  -- btxAccountId <- case HMS.lookup "fff81748-92fe-4ffa-be5a-e5407e31d1ac" accountMapping of
  --   Nothing -> do
  --     $(L.logWarn) "Couldn't find account mapping for account XYZ. Skipping entry."
  --     return ()
  --   Just a -> doWrite a

  let fAccounts :: Maybe (V.Vector FinanciusAccount) =
        _todoHole $ financiusJson ^? key "accounts" . _Array

  let transactions = fromMaybe V.empty $ financiusJson ^? key "transactions" . _Array

  let btxs :: V.Vector BluecoinTransaction = forM transactions $ \tx -> do
      case Aeson.fromJSON tx of
        Aeson.Success ftx -> Just <$> mkBluecoinTransaction conn ftx
        Aeson.Error e -> do
          $(L.logError) $ "Couldn't parse transaction: " <> show e
          return Nothing

  $(L.logInfo) "Done."

mkBluecoinTransaction :: (MonadIO io, L.MonadLogger io) => SQL.Connection -> FinanciusTransaction -> io BluecoinTransaction
mkBluecoinTransaction conn ftx = do
  let itemName = if T.null (ftxNote ftx) then "(Unnamed transaction)" else ftxNote ftx
  btxItemId <- getOrCreateItem conn itemName
  let btxAmount = ftxAmount ftx * 1000
  let btxNotes = "passyImportId:" <> ftxId ftx
  -- TODO
  let btxAccountId :: Integer = 2
  return BluecoinTransaction{..}

writeBluecoinTransaction
  :: (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> BluecoinTransaction
  -> io ()
writeBluecoinTransaction conn BluecoinTransaction{..} = do
  let sql :: SQL.Query =
        "INSERT INTO TRANSACTIONSTABLE (itemID, amount, notes, accountID, transactionCurrency, conversionRateNew, transactionTypeID, categoryID, tags, accountReference, accountPairID, uidPairID, deletedTransaction, hasPhoto, labelCount)\
        \ VALUES\
        \ (:itemID, :amount, :notes, :accountID, :transactionCurrency, :conversionRateNew, :transactionTypeID, :categoryID, :tags, :accountReference, :accountPairID, :uidPairID, :deletedTransaction, :hasPhoto, :labelCount)"
  liftIO $ SQL.executeNamed conn sql
    [ ":itemID" := btxItemId
    , ":amount" := btxAmount
    , ":notes" := btxNotes
    , ":accountID" := btxAccountId
    , ":hasPhoto" := (0 :: Int)
    , ":labelCount" := (0 :: Int)
    -- TODO
    , ":transactionCurrency" := ("GBP" :: Text)
    , ":conversionRateNew" := (1.0 :: Double)
    , ":transactionTypeID" := (fromEnum Expense)
    , ":categoryID" := (2 :: Int)
    , ":tags" := ("temptags" :: Text)
    , ":accountReference" := (3 :: Int)
    , ":accountPairID" := (123345 :: Int)
    , ":uidPairID" := (12355 :: Int)
    , ":deletedTransaction" := (6 :: Int)
    ]
