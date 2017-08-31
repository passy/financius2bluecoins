{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
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
import Control.Monad.Fail (fail)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Control.Monad.Logger as L
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Data.HashMap.Strict as HMS
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
  } deriving (Eq, Show, Generic, ParseRecord)

type AccountMapping = HMS.HashMap Text Int64

data BluecoinTransaction = BluecoinTransaction
  { btxItemId :: RowId
  , btxAmount :: Integer
  , btxNotes :: Text
  , btxDate :: Clock.UTCTime
  , btxAccount :: TransactionBundle
  , btxCategoryId :: RowId
  , btxLabels :: [Text]
  , btxConversionRate :: Double
  , btxTransactionType :: TransactionType
  } deriving (Eq, Show)

data BluecoinAccount = BluecoinAccount
  { baccId :: RowId
  , baccName :: Text
  , baccCurrencyCode :: Text
  } deriving (Eq, Show)

data BluecoinCategory = BluecoinCategory
  { bcatId :: RowId
  , bcatName :: Text
  } deriving (Eq, Show)

data FinanciusAccount = FinanciusAccount
  { faccId :: Text
  , faccCurrencyCode :: Text
  , faccName :: Text
  } deriving (Eq, Show)

data FinanciusCategory = FinanciusCategory
  { fcatId :: Text
  , fcatName :: Text
  } deriving (Eq, Show)

data FinanciusTag = FinanciusTag
  { ftagId :: Text
  , ftagName :: Text
  } deriving (Eq, Show)

data TransactionBundle = Single BluecoinAccount | Double BluecoinAccount BluecoinAccount
  deriving (Show, Eq)

instance Aeson.FromJSON FinanciusAccount where
  parseJSON = Aeson.withObject "account" $ \o ->
    FinanciusAccount
      <$> o .: "id"
      <*> o .: "currency_code"
      <*> o .: "title"

instance Aeson.FromJSON FinanciusCategory where
  parseJSON = Aeson.withObject "category" $ \o ->
    FinanciusCategory
      <$> o .: "id"
      <*> o .: "title"

instance Aeson.FromJSON FinanciusTag where
  parseJSON = Aeson.withObject "tag" $ \o ->
    FinanciusTag
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
  , ftxTagIds :: [Text]
  , ftxExchangeRate :: Double
  , ftxTransactionType :: TransactionType
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
        <*> o .: "tag_ids"
        <*> o .: "exchange_rate"
        <*> (fromFtxType =<< o .: "transaction_type")

fromFtxType :: Integer -> Aeson.Parser TransactionType
fromFtxType 1 = pure Expense
fromFtxType 2 = pure Income
fromFtxType 3 = pure Transfer
fromFtxType _ = fail "Invalid ftx transaction type"

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

instance SQL.FromRow BluecoinAccount where
  fromRow = BluecoinAccount <$> (RowId <$> SQL.field) <*> SQL.field <*> SQL.field

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

toFinanciusAccountLookupMap :: V.Vector FinanciusAccount -> HMS.HashMap Text FinanciusAccount
toFinanciusAccountLookupMap =
  toLookupMap (\acc@FinanciusAccount{..} -> (faccName, acc))

toFinanciusTagLookupMap :: V.Vector FinanciusTag -> HMS.HashMap Text FinanciusTag
toFinanciusTagLookupMap =
  toLookupMap (\tag@FinanciusTag{..} -> (ftagId, tag))

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
  conn <- liftIO . SQL.open $ bluecoinFile args


  -- I'm sure there's a better way for this. I must be ignoring some useful law here.
  let fAccounts :: HMS.HashMap Text FinanciusAccount =
        maybe (error "parsing accounts failed") toFinanciusAccountLookupMap (decodeJSONArray "accounts" financiusJson)
  bAccounts <- getBluecoinAccounts conn
  mergedAccounts :: HMS.HashMap Text BluecoinAccount <- mergeAccounts bAccounts fAccounts

  let fCategories :: HMS.HashMap Text FinanciusCategory =
        maybe (error "parsing accounts failed") toFinanciusCategoryLookupMap (decodeJSONArray "categories" financiusJson)
  bCategories <- getBluecoinCategories conn
  mergedCategories :: HMS.HashMap Text BluecoinCategory <- mergeCategories bCategories fCategories

  let fTags :: HMS.HashMap Text FinanciusTag =
        maybe (error "parsing tags failed") (toFinanciusTagLookupMap) (decodeJSONArray "tags" financiusJson)

  let transactions = fromMaybe V.empty $ financiusJson ^? key "transactions" . _Array

  maybeBtxs :: V.Vector (Maybe BluecoinTransaction) <- forM transactions $ \tx -> do
      case Aeson.fromJSON tx of
        Aeson.Success ftx -> runMaybeT $
          mkBluecoinTransaction conn mergedAccounts mergedCategories fTags ftx
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
mergeCategories =
  mergeLookupMaps bcatName fcatId

mergeAccounts
  :: L.MonadLogger m
  => [BluecoinAccount]
  -> HMS.HashMap Text FinanciusAccount
  -> m (HMS.HashMap Text BluecoinAccount)
mergeAccounts =
  mergeLookupMaps baccName faccId

mergeLookupMaps
  :: forall a b (m :: * -> *).
    (Eq a, Eq b, L.MonadLogger m)
  => (b -> Text)
  -> (a -> Text)
  -> [b]
  -> HMS.HashMap Text a
  -> m (HMS.HashMap Text b)
mergeLookupMaps extractName extractKey list' lookupMap = do
  vals <- catMaybes <$> mapM extract list'
  return $ HMS.fromList vals
  where
    extract
      :: b
      -> m (Maybe (Text, b))
    extract element' =
      case HMS.lookup (extractName element') lookupMap of
        Just res -> return $ Just (extractKey res, element')
        Nothing -> do
          $(L.logError) $ "Could not find corresponding lookup element '" <> extractName element' <> "'."
          return Nothing

getBluecoinCategories
  :: MonadIO io
  => SQL.Connection
  -> io [BluecoinCategory]
getBluecoinCategories conn =
  liftIO $ SQL.query_ conn "SELECT categoryTableID, childCategoryName FROM CHILDCATEGORYTABLE"

getBluecoinAccounts
  :: MonadIO io
  => SQL.Connection
  -> io [BluecoinAccount]
getBluecoinAccounts conn =
  liftIO $ SQL.query_ conn "SELECT accountsTableID, accountName, accountCurrency FROM ACCOUNTSTABLE"

decodeValueEither :: (Aeson.FromJSON a) => Aeson.Value -> Either Text a
decodeValueEither v = case Aeson.fromJSON v of
  Aeson.Success res -> pure res
  Aeson.Error e -> Left $ T.pack e

getBtxAccount
  :: L.MonadLogger m
  => HMS.HashMap Text BluecoinAccount
  -> FinanciusTransaction
  -> MaybeT m TransactionBundle
getBtxAccount baccs FinanciusTransaction{..} = do
  -- My Lord, this turned out really nicely.
  case ftxTransactionType of
    Expense -> Single <$> lookup ftxAccountFromId
    Income -> Single <$> lookup ftxAccountToId
    Transfer -> Double <$> lookup ftxAccountFromId <*> lookup ftxAccountToId

  where
    lookup
      :: L.MonadLogger m
      => Maybe Text
      -> MaybeT m BluecoinAccount
    lookup field =
      do MaybeT $ case (flip HMS.lookup) baccs =<< field of
          Nothing | isJust ftxAccountFromId -> ($(L.logError) $ "Invariant violation: account not in mapping list: " <> show field) >> pure Nothing
                  | otherwise -> pure Nothing
          Just btxAccount -> pure $ Just btxAccount

mkBluecoinTransaction
  :: (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> HMS.HashMap Text BluecoinAccount
  -> HMS.HashMap Text BluecoinCategory
  -> HMS.HashMap Text FinanciusTag
  -> FinanciusTransaction
  -> MaybeT io BluecoinTransaction
mkBluecoinTransaction conn baccs bcats ftags ftx@FinanciusTransaction{..} = do
  let itemName =
        if T.null ftxNote
          then "(Unnamed transaction)"
          else ftxNote
  btxItemId :: RowId <- getOrCreateItem conn itemName
  let btxNotes = "passyImportId:" <> ftxId
  let btxAmount = ftxAmount * 10000
  let btxDate = PClock.posixSecondsToUTCTime $ realToFrac $ ftxDate `div` 1000
  let btxLabels :: [Text] = ftagName <$> catMaybes (flip HMS.lookup ftags <$> ftxTagIds)
  let btxConversionRate = ftxExchangeRate
  let btxTransactionType = ftxTransactionType

  btxAccount <- getBtxAccount baccs ftx

  -- FIXME: This doesn't actually fall back.
  btxCategoryId <- MaybeT $ case (flip HMS.lookup) bcats (fromMaybe transferCategoryName ftxCategoryId) of
    Nothing -> ($(L.logError) $ "Invariant violation: category id not populated: " <> show ftxCategoryId) >> pure Nothing
    Just BluecoinCategory{..} -> pure $ Just bcatId

  return $ BluecoinTransaction{..}

mkBluecoinAccount :: AccountMapping -> FinanciusAccount -> Maybe BluecoinAccount
mkBluecoinAccount accountMapping FinanciusAccount{..} = do
  let baccCurrencyCode = faccCurrencyCode
  let baccName = faccName
  baccId <- RowId <$> HMS.lookup faccId accountMapping
  pure BluecoinAccount{..}

writeBluecoinTransaction
  :: forall (io :: * -> *)
  .  (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> BluecoinTransaction
  -> io ()
writeBluecoinTransaction conn BluecoinTransaction{..} = do
  let amount = case btxTransactionType of
        Expense -> negate btxAmount
        Income -> btxAmount
        -- TODO
        Transfer -> 0

  -- FIXME: This obviously ignores half of the transaction at the moment.
  account <- case btxAccount of
        Single a -> pure a
        Double a _ ->
          ($(L.logError) "FIXME - Omitting half of the transaction. Woops.") >> pure a

  write amount account

  where
    write
      :: Integer
      -> BluecoinAccount
      -> io ()
    write amount account = do
      let sql :: SQL.Query =
            "INSERT INTO TRANSACTIONSTABLE (itemID, amount, notes, accountID, transactionCurrency, conversionRateNew, transactionTypeID, categoryID, tags, accountReference, accountPairID, uidPairID, deletedTransaction, hasPhoto, labelCount, date)\
            \ VALUES\
            \ (:itemID, :amount, :notes, :accountID, :transactionCurrency, :conversionRateNew, :transactionTypeID, :categoryID, :tags, :accountReference, :accountPairID, :uidPairID, :deletedTransaction, :hasPhoto, :labelCount, :date)"
      liftIO $ SQL.executeNamed conn sql
        [ ":itemID" := btxItemId
        , ":amount" := amount
        , ":notes" := btxNotes
        , ":date" := btxDate
        , ":categoryID" := btxCategoryId
        , ":hasPhoto" := (0 :: Int)
        , ":labelCount" := length btxLabels
        , ":conversionRateNew" := btxConversionRate
        , ":transactionTypeID" := fromEnum btxTransactionType
        , ":uidPairID" := (-1 :: Int)
        , ":accountID" := baccId account
        , ":accountPairID" := baccId account
        , ":transactionCurrency" := baccCurrencyCode account
        , ":accountReference" := (3 :: Int)
        , ":deletedTransaction" := (6 :: Int)
        -- TODO
        , ":tags" := ("temptags" :: Text)
        ]
      rowId <- liftIO $ RowId <$> SQL.lastInsertRowId conn
      setTxPairId conn rowId rowId
      mapM_ (writeBluecoinLabel conn rowId) btxLabels

writeBluecoinLabel
  :: (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> RowId
  -> Text
  -> io ()
writeBluecoinLabel conn (RowId rowId) label = do
  let sql :: SQL.Query =
        "INSERT INTO LABELSTABLE (labelName, transactionIDLabels)\
        \ VALUES\
        \ (:label, :rowId)"
  liftIO $ SQL.executeNamed conn sql
     [ ":label" := label
     , ":rowId" := rowId ]

setTxPairId
  :: (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> RowId
  -> RowId
  -> io ()
setTxPairId conn txId pairId = do
  let sql :: SQL.Query =
        "UPDATE TRANSACTIONSTABLE SET uidPairID = :pairId WHERE transactionsTableID = :txId"
  liftIO $ SQL.executeNamed conn sql
    [ ":txId" := txId
    , ":uidPairID" := pairId
    ]
