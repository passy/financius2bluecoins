{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-deprecations #-}

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

type AccountMapping = HMS.HashMap Text Int64

data BluecoinTransaction = BluecoinTransaction
  { btxItemId :: RowId
  , btxAmount :: Integer
  , btxNotes :: Text
  , btxAccount :: BluecoinAccount
  } deriving (Eq, Show)

data BluecoinAccount = BluecoinAccount
  { baccId :: RowId
  , baccCurrencyCode :: Text
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

vecCatMaybes :: V.Vector (Maybe a) -> V.Vector a
vecCatMaybes = V.concatMap f
  where
        f :: forall a. Maybe a -> V.Vector a
        f (Just a) = V.singleton a
        f Nothing = V.empty

main :: IO ()
main = L.runStderrLoggingT $ do
  $(L.logInfo) "Getting started ..."
  args :: Args <- getRecord "financius2bluecoin"
  financiusJson <- liftIO . readFile $ financiusFile args
  mappingJson <- liftIO . readFile $ mappingFile args
  conn <- liftIO . SQL.open $ bluecoinFile args

  let accountMapping :: AccountMapping =
        case first T.pack . Aeson.eitherDecodeStrict $ TE.encodeUtf8 mappingJson of
          Left e -> error $ "mapping parsing failed: " <> e
          Right a -> a

  -- I'm sure there's a better way for this. I must be ignoring some useful law here.
  let fAccounts :: V.Vector FinanciusAccount =
        case sequenceA $ join <$> sequenceA (fmap (hush . decodeValueEither) <$> (financiusJson ^? key "accounts" . _Array)) of
          Nothing -> error $ "parsing accounts failed"
          Just a -> a
  mergedAccounts :: HMS.HashMap Text BluecoinAccount <- mergeAccounts accountMapping fAccounts

  let transactions = fromMaybe V.empty $ financiusJson ^? key "transactions" . _Array

  maybeBtxs :: V.Vector (Maybe BluecoinTransaction) <- forM transactions $ \tx -> do
      case Aeson.fromJSON tx of
        Aeson.Success ftx -> mkBluecoinTransaction conn mergedAccounts ftx
        Aeson.Error e -> do
          $(L.logError) $ "Couldn't parse transaction: " <> show e
          return Nothing

  _ <- mapM (writeBluecoinTransaction conn) (vecCatMaybes maybeBtxs)

  $(L.logInfo) "Done."

mergeAccounts :: L.MonadLogger m
  => AccountMapping
  -> V.Vector FinanciusAccount
  -> m (HMS.HashMap Text BluecoinAccount)
mergeAccounts accountMapping fAccounts =
  let fAccounts' :: HMS.HashMap Text FinanciusAccount =
        HMS.fromList .
        V.toList $
        V.map (\facc@FinanciusAccount {faccId} -> (faccId, facc)) fAccounts
      bAccounts :: HMS.HashMap Text (Maybe BluecoinAccount)
      bAccounts = mkBluecoinAccount accountMapping <$> fAccounts'
  in do
    forM_ (HMS.keys $ HMS.filter isNothing bAccounts) $ \k -> $(L.logError) $ "Could not find mapping for account <" <> show k <> ">."
    return $ HMS.mapMaybe identity bAccounts

decodeValueEither :: (Aeson.FromJSON a) => Aeson.Value -> Either Text a
decodeValueEither v = case Aeson.fromJSON v of
  Aeson.Success res -> pure res
  Aeson.Error e -> Left $ T.pack e

mkBluecoinTransaction
  :: (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> HMS.HashMap Text BluecoinAccount
  -> FinanciusTransaction
  -> io (Maybe BluecoinTransaction)
mkBluecoinTransaction conn baccs FinanciusTransaction {..} = do
  let itemName =
        if T.null ftxNote
          then "(Unnamed transaction)"
          else ftxNote
  btxItemId <- getOrCreateItem conn itemName
  let btxAmount = ftxAmount * 1000
  let btxNotes = "passyImportId:" <> ftxId

  -- TODO: Investigate out how ToId is used.
  case (flip HMS.lookup) baccs =<< ftxAccountFromId of
        Nothing -> ($(L.logError) $ "Invariant violation: fromAccountId not in accounts list: " <> show ftxAccountFromId) >> pure Nothing
        Just btxAccount -> pure $ Just BluecoinTransaction{..}

mkBluecoinAccount :: AccountMapping -> FinanciusAccount -> Maybe BluecoinAccount
mkBluecoinAccount accountMapping FinanciusAccount{..} = do
  let baccCurrencyCode = faccCurrencyCode
  baccId <- RowId <$> HMS.lookup faccId accountMapping
  pure BluecoinAccount{..}

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
    , ":accountID" := baccId btxAccount
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
