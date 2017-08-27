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
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

import qualified Data.Aeson as Aeson
import qualified Control.Monad.Logger as L
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as PClock
import qualified Data.Hashable as Hashable

-- * Constants

transferCategoryName :: Text
transferCategoryName = "(Transfer)"

-- * Definitions

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
  , btxDate :: Clock.UTCTime
  , btxAccount :: BluecoinAccount
  , btxCategoryId :: RowId
  } deriving (Eq, Show)

data BluecoinAccount = BluecoinAccount
  { baccId :: RowId
  , baccCurrencyCode :: Text
  } deriving (Eq, Show)

data BluecoinCategory = BluecoinCategory
  { bcatId :: RowId
  , bcatName :: Text
  }

data FinanciusAccount = FinanciusAccount
  { faccId :: Text
  , faccCurrencyCode :: Text
  } deriving (Eq, Show)

data FinanciusCategory = FinanciusCategory
  { fcatId :: Text
  , fcatName :: Text
  } deriving (Eq, Show)

instance Aeson.FromJSON FinanciusAccount where
  parseJSON = Aeson.withObject "account" $ \o ->
    FinanciusAccount
      <$> o .: "id"
      <*> o .: "currency_code"

instance Aeson.FromJSON FinanciusCategory where
  parseJSON = Aeson.withObject "category" $ \o ->
    FinanciusCategory
      <$> o .: "id"
      <*> o .: "title"

data FinanciusTransaction = FinanciusTransaction
  { ftxId :: Text
  , ftxAccountFromId :: Maybe Text
  , ftxAccountToId :: Maybe Text
  , ftxNote :: Text
  , ftxAmount :: Integer
  , ftxDate :: Integer
  , ftxCategoryId :: Maybe Text
  } deriving (Eq, Show)

instance Aeson.FromJSON FinanciusTransaction where
  parseJSON = Aeson.withObject "transaction" $ \o ->
    FinanciusTransaction
        <$> o .: "id"
        <*> o .:? "account_from_id"
        <*> o .:? "account_to_id"
        <*> o .: "note"
        <*> o .: "amount"
        <*> o .: "date"
        <*> o .: "category_id"

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

instance SQL.FromRow BluecoinCategory where
  fromRow = BluecoinCategory <$> (RowId <$> SQL.field) <*> SQL.field

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

toLookupMap ::
  (Hashable.Hashable b, Eq b) =>
  (a -> (b, a)) ->
  V.Vector a ->
  HMS.HashMap b a
toLookupMap extract =
  HMS.fromList . V.toList . V.map extract

toFinanciusCategoryLookupMap :: V.Vector FinanciusCategory -> HMS.HashMap Text FinanciusCategory
toFinanciusCategoryLookupMap =
  toLookupMap (\cat@FinanciusCategory{..} -> (fcatName, cat))

vecCatMaybes :: V.Vector (Maybe a) -> V.Vector a
vecCatMaybes = V.concatMap f
  where
        f :: forall a. Maybe a -> V.Vector a
        f (Just a) = V.singleton a
        f Nothing = V.empty

decodeJSONArray
  :: (AsValue s, Aeson.FromJSON a)
  => Text
  -> s
  -> Maybe (V.Vector a)
decodeJSONArray key_ json =
  sequenceA $ join <$> sequenceA (fmap (hush . decodeValueEither) <$> (json ^? key key_. _Array))

main :: IO ()
main = L.runStderrLoggingT $ do
  $(L.logInfo) "Getting started ..."
  args :: Args <- getRecord "financius2bluecoin"
  financiusJson <- liftIO . readFile $ financiusFile args
  mappingJson <- liftIO . readFile $ mappingFile args
  conn <- liftIO . SQL.open $ bluecoinFile args

  let accountMapping :: AccountMapping =
        case first T.pack . Aeson.eitherDecodeStrict $ TE.encodeUtf8 mappingJson of
          Left e -> error $ "parsing mapping file " <> show (mappingFile args) <> " failed: " <> e
          Right a -> a

  -- I'm sure there's a better way for this. I must be ignoring some useful law here.
  let fAccounts :: V.Vector FinanciusAccount =
        fromMaybe (error "parsing accounts failed") (decodeJSONArray "accounts" financiusJson)

  mergedAccounts :: HMS.HashMap Text BluecoinAccount <- mergeAccounts accountMapping fAccounts

  let fCategories :: HMS.HashMap Text FinanciusCategory =
        maybe (error "parsing accounts failed") (toFinanciusCategoryLookupMap) (decodeJSONArray "categories" financiusJson)
  bCategories <- getBluecoinCategories conn
  mergedCategories :: HMS.HashMap Text BluecoinCategory <- mergeCategories bCategories fCategories

  let transactions = fromMaybe V.empty $ financiusJson ^? key "transactions" . _Array

  maybeBtxs :: V.Vector (Maybe BluecoinTransaction) <- forM transactions $ \tx -> do
      case Aeson.fromJSON tx of
        Aeson.Success ftx -> runMaybeT $ mkBluecoinTransaction conn mergedAccounts mergedCategories ftx
        Aeson.Error e -> do
          $(L.logError) $ "Couldn't parse transaction: " <> show e
          return Nothing

  $(L.logInfo) "Writing transactions ..."
  mapM_ (writeBluecoinTransaction conn) (vecCatMaybes maybeBtxs)

  $(L.logInfo) "Done."


mergeCategories
  :: L.MonadLogger m
  => [BluecoinCategory]
  -> HMS.HashMap Text FinanciusCategory
  -> m (HMS.HashMap Text BluecoinCategory)
mergeCategories bCategories fCategories = do
  vals <- catMaybes <$> mapM extract bCategories
  return $ HMS.fromList vals
  where
    extract
      :: L.MonadLogger m
      => BluecoinCategory
      -> m (Maybe (Text, BluecoinCategory))
    extract bcat@BluecoinCategory{..} =
      case HMS.lookup bcatName fCategories of
        Just FinanciusCategory{..} -> return $ Just (fcatId, bcat)
        Nothing -> do
          $(L.logError) $ "Could not find corresponding Financius category for '" <> bcatName <> "'."
          return Nothing

getBluecoinCategories
  :: MonadIO io
  => SQL.Connection
  -> io [BluecoinCategory]
getBluecoinCategories conn =
  liftIO $ SQL.query_ conn "SELECT categoryTableID, childCategoryName FROM CHILDCATEGORYTABLE"

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
  -> HMS.HashMap Text BluecoinCategory
  -> FinanciusTransaction
  -> MaybeT io BluecoinTransaction
mkBluecoinTransaction conn baccs bcats FinanciusTransaction{..} = do
  let itemName =
        if T.null ftxNote
          then "(Unnamed transaction)"
          else ftxNote
  btxItemId :: RowId <- getOrCreateItem conn itemName
  let btxAmount = ftxAmount * 10000
  let btxNotes = "passyImportId:" <> ftxId
  let btxDate = PClock.posixSecondsToUTCTime $ realToFrac $ ftxDate `div` 1000

  -- TODO: Investigate out how ToId is used.
  btxAccount <- MaybeT $ case (flip HMS.lookup) baccs =<< ftxAccountFromId of
    Nothing | isJust ftxAccountFromId -> ($(L.logError) $ "Invariant violation: fromAccountId not in accounts list: " <> show ftxAccountFromId) >> pure Nothing
            | otherwise -> pure Nothing
    Just btxAccount -> pure $ Just btxAccount

  btxCategoryId <- MaybeT $ case (flip HMS.lookup) bcats (fromMaybe transferCategoryName ftxCategoryId) of
    Nothing -> ($(L.logError) $ "Invariant violation: category id not populated: " <> show ftxCategoryId) >> pure Nothing
    Just BluecoinCategory{..} -> pure $ Just bcatId

  return $ BluecoinTransaction{..}

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
        "INSERT INTO TRANSACTIONSTABLE (itemID, amount, notes, accountID, transactionCurrency, conversionRateNew, transactionTypeID, categoryID, tags, accountReference, accountPairID, uidPairID, deletedTransaction, hasPhoto, labelCount, date)\
        \ VALUES\
        \ (:itemID, :amount, :notes, :accountID, :transactionCurrency, :conversionRateNew, :transactionTypeID, :categoryID, :tags, :accountReference, :accountPairID, :uidPairID, :deletedTransaction, :hasPhoto, :labelCount, :date)"
  liftIO $ SQL.executeNamed conn sql
    [ ":itemID" := btxItemId
    , ":amount" := btxAmount
    , ":notes" := btxNotes
    , ":date" := btxDate
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
    , ":accountPairID" := baccId btxAccount
    , ":uidPairID" := (-1 :: Int)
    , ":deletedTransaction" := (6 :: Int)
    ]
  updateLastTransaction conn

-- | Transactions are self-referential, so we need to update written transactions once.
-- If you run this without having inserted a transaction before, you'll be in big trouble.
updateLastTransaction
  :: (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> io ()
updateLastTransaction conn = do
  let sql :: SQL.Query =
        "UPDATE TRANSACTIONSTABLE SET uidPairID = :lastId WHERE transactionsTableID = :lastId"

  lastRowId <- liftIO $ SQL.lastInsertRowId conn
  liftIO $ SQL.executeNamed conn sql
    [ ":lastId" := lastRowId
    ]
