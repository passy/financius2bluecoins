{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-deprecations #-}

module Main where

import Protolude
import Data.Aeson.Lens
import Control.Lens
import Options.Generic (ParseRecord, ParseField(..), ParseFields, getRecord)
import Data.Aeson ((.:), (.:?))
import Database.SQLite.Simple (NamedParam((:=)))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Fail (fail)

import qualified Codec.Archive.Zip as Zip
import qualified Control.Monad.Logger as L
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HMS
import qualified Data.Hashable as Hashable
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TRead
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as PClock
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Options.Applicative as Options

-- * Constants

transferCategory :: BluecoinCategory
transferCategory = BluecoinCategory (RowId 3) "(Transfer)"

newAccountCategory :: BluecoinCategory
newAccountCategory = BluecoinCategory (RowId 2) "(New Account)"

-- * Parsing utils

parseHelpfulString
  :: String.String
  -> Maybe Text
  -> Maybe Text
  -> Maybe Char
  -> Options.Parser Text
parseHelpfulString metavar h m c =
    case m of
        Nothing   -> do
            let fs :: Options.Mod Options.ArgumentFields a
                fs =  Options.metavar metavar
                   <> foldMap (Options.help . T.unpack) h
            Options.argument (T.pack <$> Options.str) fs
        Just name -> do
            let fs :: Options.Mod Options.OptionFields a
                fs =  Options.metavar metavar
                   <> Options.long (T.unpack name)
                   <> foldMap (Options.help . T.unpack) h
                   <> foldMap Options.short c
            Options.option (T.pack <$> Options.str) fs

-- * Definitions

data Args = Args
  { financiusFile :: FilePath
  , bluecoinFile :: FilePath
  , baseCurrency :: CurrencyCode
  , eurofxrefFile :: Maybe FilePath
  } deriving (Eq, Show, Generic, ParseRecord)

type AccountMapping = HMS.HashMap Text Int64

type FxTable = HMS.HashMap FxDate FxRecord

type FxLookup = (FxDate -> Maybe FxRecord)

data BluecoinTransaction = BluecoinTransaction
  { btxItemId :: RowId
  , btxAmount :: Integer
  , btxNotes :: Text
  , btxDate :: Clock.UTCTime
  , btxAccount :: TransactionBundle (BluecoinAccount, Double)
  , btxCategoryId :: RowId
  , btxLabels :: [Text]
  , btxConversionRate :: Double
  , btxTransactionType :: BluecoinTransactionType
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

newtype FxDate = FxDate Calendar.Day
  deriving (Eq, Show, Ord)

newtype CurrencyCode = CurrencyCode Text
  deriving (Eq, Show, Generic, Typeable, ParseFields, ParseRecord)

-- * Instances

instance ParseField CurrencyCode where
  parseField h m c = CurrencyCode <$> parseHelpfulString "CURRENCY_CODE" h m c

instance Enum FxDate where
  toEnum = FxDate . toEnum
  fromEnum (FxDate d) = fromEnum d

instance Hashable.Hashable FxDate where
  hash (FxDate d) =
    let (x, y, z) = Calendar.toGregorian d
    in fromIntegral $ 32 * x + 32 * fromIntegral y + 32 * fromIntegral z

  hashWithSalt salt v = Hashable.hash v * salt

-- | A fx rate record for a given day, based on the ECB CSV data.
data FxRecord = FxRecord
  { fxDate :: FxDate
  , fxRates :: HMS.HashMap Text Double
  } deriving (Eq, Show)

instance Csv.FromField FxDate where
  parseField v' = parseEither $
        let (year, v'') = T.break (== '-') $ decodeUtf8 v'
            (month, v''') = T.break (== '-') $ T.drop 1 v''
            (day, _) = T.break (== '-') $ T.drop 1 v'''
        in FxDate <$> (Calendar.fromGregorian <$> dec year <*> dec month <*> dec day)

    where
      dec :: forall b. Integral b => Text -> Either String.String b
      dec = (fst <$>) . TRead.decimal

      parseEither :: Either a b -> Csv.Parser b
      parseEither = \case
        Left _ -> mzero
        Right b -> pure b

instance Csv.FromNamedRecord FxRecord where
  parseNamedRecord v =
    FxRecord <$> (v Csv..: "Date") <*> eitherToParser (parseFxRatesTable v)

eitherToParser :: Either Text b -> Csv.Parser b
eitherToParser (Left _) = mempty
eitherToParser (Right b) = pure b

parseFxRatesTable :: HMS.HashMap ByteString ByteString -> Either Text (HMS.HashMap Text Double)
parseFxRatesTable = first T.pack . sequence . HMS.fromList . catMaybes . fmap transform' . HMS.toList
  where
    transform' :: (ByteString, ByteString) -> Maybe (Text, Either String.String Double)
    transform' (k, v) | v == "N/A" = Nothing
                      | k == "Date" = Nothing
                      | k == mempty || v == mempty = Nothing
                      | otherwise  = pure (TE.decodeUtf8 k, fst <$> (TRead.double . TE.decodeUtf8 $ v))

data TransactionBundle a = Single a | Double a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

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
  , ftxTransactionType :: FinanciusTransactionType
  , ftxModelState :: FinanciusModelState
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
        <*> (fromFtxModelState =<< o .: "model_state")

fromFtxType :: Integer -> Aeson.Parser FinanciusTransactionType
fromFtxType 1 = pure FtxExpense
fromFtxType 2 = pure FtxIncome
fromFtxType 3 = pure FtxTransfer
fromFtxType _ = fail "Invalid ftx transaction type"

fromFtxModelState :: Integer -> Aeson.Parser FinanciusModelState
fromFtxModelState 1 = pure FtxModelStateValid
fromFtxModelState 2 = pure FtxModelStateInvalid
fromFtxModelState _ = pure FtxModelStateUnknown

data FinanciusTransactionType = FtxTransfer | FtxIncome | FtxExpense
  deriving (Eq, Show)

data FinanciusModelState = FtxModelStateValid | FtxModelStateInvalid | FtxModelStateUnknown
  deriving (Eq, Show)

data BluecoinTransactionType = BtxTransfer | BtxIncome | BtxExpense | BtxNewAccount
  deriving (Eq, Show)

instance Enum BluecoinTransactionType where
  fromEnum BtxTransfer = 5
  fromEnum BtxIncome = 4
  fromEnum BtxExpense = 3
  fromEnum BtxNewAccount = 2

  toEnum 5 = BtxTransfer
  toEnum 4 = BtxIncome
  toEnum 3 = BtxExpense
  toEnum 2 = BtxNewAccount
  toEnum i = error $ "Invalid transaction type " <> show i

ftxToBtxType :: FinanciusTransactionType -> BluecoinTransactionType
ftxToBtxType FtxTransfer = BtxTransfer
ftxToBtxType FtxIncome   = BtxIncome
ftxToBtxType FtxExpense  = BtxExpense

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
    Just a  -> return a
    Nothing -> liftIO $ do
      createFn conn
      RowId <$> SQL.lastInsertRowId conn

getOrCreateItem :: MonadIO io => SQL.Connection -> Text -> io RowId
getOrCreateItem conn name = getOrCreate
  conn
  ( \c -> SQL.query c
                    "SELECT itemTableId FROM ITEMTABLE WHERE itemName = ?"
                    (SQL.Only name)
  )
  ( \c -> SQL.execute c
                      "INSERT INTO ITEMTABLE (itemName) VALUES (?)"
                      (SQL.Only name)
  )

toLookupMap
  :: (Hashable.Hashable b, Eq b)
  => (a -> (b, a))
  -> V.Vector a
  -> HMS.HashMap b a
toLookupMap extract = HMS.fromList . V.toList . V.map extract

toFinanciusCategoryLookupMap
  :: V.Vector FinanciusCategory -> HMS.HashMap Text FinanciusCategory
toFinanciusCategoryLookupMap =
  toLookupMap (\cat@FinanciusCategory {..} -> (fcatName, cat))

toFinanciusAccountLookupMap
  :: V.Vector FinanciusAccount -> HMS.HashMap Text FinanciusAccount
toFinanciusAccountLookupMap =
  toLookupMap (\acc@FinanciusAccount {..} -> (faccName, acc))

toFinanciusTagLookupMap
  :: V.Vector FinanciusTag -> HMS.HashMap Text FinanciusTag
toFinanciusTagLookupMap = toLookupMap (\tag@FinanciusTag {..} -> (ftagId, tag))

vecCatMaybes :: V.Vector (Maybe a) -> V.Vector a
vecCatMaybes = V.concatMap f
 where
  f :: forall a . Maybe a -> V.Vector a
  f (Just a) = V.singleton a
  f Nothing  = V.empty

decodeJSONArray
  :: (AsValue s, Aeson.FromJSON a) => Text -> s -> Maybe (V.Vector a)
decodeJSONArray key_ json =
  foldMap (hush . decodeValueEither) <$> (json ^? key key_ . _Array)

readFxRefFile :: FilePath -> IO (Maybe BSL.ByteString)
readFxRefFile path = do
  archive <- Zip.toArchive <$> BSL.readFile path
  return $ extractCSV archive
 where
  extractCSV = fmap Zip.fromEntry . Zip.findEntryByPath "eurofxref-hist.csv"

loadFxRates :: (MonadIO m, L.MonadLogger m) => FilePath -> m FxLookup
loadFxRates file = do
  fxRef :: Maybe BSL.ByteString <- liftIO . readFxRefFile $ file
  let fxData :: Maybe (V.Vector FxRecord) = foldMap (fmap snd . hush . Csv.decodeByName) fxRef
  let table :: Maybe FxTable = foldl' (\b a@FxRecord{..} -> HMS.insert fxDate a b) HMS.empty <$> fxData

  case table of
    Just t -> return $ \k -> HMS.lookup k t
    Nothing -> do
      $(L.logWarn) "Decoding ECB Euro exchange rates failed."
      return $ const Nothing

-- | A very terrible attempt of "always" getting a date, by simply going back by up to four
-- days if one is missing in the list. This may seem silly, but we actually know the data, so
-- this is fine. And while it would be more accurate to lineraly interpolate, it's also
-- overkill.
backtrackFxRate :: FxLookup -> FxDate -> Maybe FxRecord
backtrackFxRate lookup = go 5
  where
    go :: Int -> FxDate -> Maybe FxRecord
    go 0 _ = Nothing
    go i fxd =
      case lookup fxd of
        Just r -> Just r
        Nothing -> go (pred i) (pred fxd)

main :: IO ()
main = L.runStderrLoggingT $ do
  $(L.logInfo) "Getting started ..."
  args :: Args                  <- getRecord "financius2bluecoin"
  financiusJson                 <- liftIO . readFile $ financiusFile args
  conn                          <- liftIO . SQL.open $ bluecoinFile args
  fxLookup' :: Maybe FxLookup   <- traverse loadFxRates (eurofxrefFile args)
  fxLookup <- case fxLookup' of
        Just a -> pure a
        Nothing -> do
          $(L.logWarn) "No fxrates provided. Falling back to 1.0 rate."
          return $ const Nothing

  $(L.logInfo) "Parsing inputs complete."

  -- I'm sure there's a better way for this. I must be ignoring some useful law here.
  let fAccounts :: HMS.HashMap Text FinanciusAccount = maybe
        (error "parsing accounts failed")
        toFinanciusAccountLookupMap
        (decodeJSONArray "accounts" financiusJson)
  bAccounts <- getBluecoinAccounts conn
  mergedAccounts :: HMS.HashMap Text BluecoinAccount <- mergeAccounts
    bAccounts
    fAccounts

  let fCategories :: HMS.HashMap Text FinanciusCategory = maybe
        (error "parsing accounts failed")
        toFinanciusCategoryLookupMap
        (decodeJSONArray "categories" financiusJson)
  bCategories <- getBluecoinCategories conn
  mergedCategories :: HMS.HashMap Text BluecoinCategory <- mergeCategories
    bCategories
    fCategories

  let fTags :: HMS.HashMap Text FinanciusTag = maybe
        (error "parsing tags failed")
        toFinanciusTagLookupMap
        (decodeJSONArray "tags" financiusJson)

  let transactions =
        fromMaybe V.empty $ financiusJson ^? key "transactions" . _Array

  maybeFtxs :: V.Vector (Maybe FinanciusTransaction) <-
    forM transactions $ \tx -> case Aeson.fromJSON tx of
      Aeson.Success (ftx@FinanciusTransaction { ftxModelState })
        | ftxModelState /= FtxModelStateInvalid -> return $ Just ftx
        | otherwise                             -> return Nothing
      Aeson.Error e -> do
        $(L.logError) $ "Couldn't parse transaction: " <> show e
        return Nothing

  maybeBtxs :: V.Vector (Maybe BluecoinTransaction) <-
    fmap join
      <$> V.mapM
            ( \m -> forM
              m
              ( runMaybeT
              . mkBluecoinTransaction conn
                                      (baseCurrency args)
                                      mergedAccounts
                                      mergedCategories
                                      fTags
                                      fxLookup
              )
            )
            maybeFtxs

  $(L.logInfo) "Writing transactions ..."
  mapM_ (writeBluecoinTransaction conn) (vecCatMaybes maybeBtxs)

  $(L.logInfo) "Done."

mergeCategories
  :: L.MonadLogger m
  => [BluecoinCategory]
  -> HMS.HashMap Text FinanciusCategory
  -> m (HMS.HashMap Text BluecoinCategory)
mergeCategories = mergeLookupMaps bcatName fcatId

mergeAccounts
  :: L.MonadLogger m
  => [BluecoinAccount]
  -> HMS.HashMap Text FinanciusAccount
  -> m (HMS.HashMap Text BluecoinAccount)
mergeAccounts = mergeLookupMaps baccName faccId

mergeLookupMaps
  :: forall a b (m :: * -> *)
   . (Eq a, Eq b, L.MonadLogger m)
  => (b -> Text)
  -> (a -> Text)
  -> [b]
  -> HMS.HashMap Text a
  -> m (HMS.HashMap Text b)
mergeLookupMaps extractName extractKey list' lookupMap = do
  vals <- catMaybes <$> mapM extract list'
  return $ HMS.fromList vals
 where
  extract :: b -> m (Maybe (Text, b))
  extract element' = case HMS.lookup (extractName element') lookupMap of
    Just res -> return $ Just (extractKey res, element')
    Nothing  -> do
      $(L.logError)
        $  "Could not find corresponding lookup element '"
        <> extractName element'
        <> "'."
      return Nothing

getBluecoinCategories :: MonadIO io => SQL.Connection -> io [BluecoinCategory]
getBluecoinCategories conn = liftIO $ SQL.query_
  conn
  "SELECT categoryTableID, childCategoryName FROM CHILDCATEGORYTABLE"

getBluecoinAccounts :: MonadIO io => SQL.Connection -> io [BluecoinAccount]
getBluecoinAccounts conn = liftIO $ SQL.query_
  conn
  "SELECT accountsTableID, accountName, accountCurrency FROM ACCOUNTSTABLE"

decodeValueEither :: (Aeson.FromJSON a) => Aeson.Value -> Either Text a
decodeValueEither v = case Aeson.fromJSON v of
  Aeson.Success res -> pure res
  Aeson.Error   e   -> Left $ T.pack e

-- | My Lord, this turned out really nicely.
getBtxAccount
  :: L.MonadLogger m
  => HMS.HashMap Text BluecoinAccount
  -> FinanciusTransaction
  -> MaybeT m (TransactionBundle BluecoinAccount)
getBtxAccount baccs FinanciusTransaction {..} = case ftxTransactionType of
  FtxExpense  -> Single <$> lookup ftxAccountFromId
  FtxIncome   -> Single <$> lookup ftxAccountToId
  FtxTransfer -> Double <$> lookup ftxAccountFromId <*> lookup ftxAccountToId
 where
  lookup :: L.MonadLogger m => Maybe Text -> MaybeT m BluecoinAccount
  lookup field = MaybeT $ case flip HMS.lookup baccs =<< field of
    Nothing
      | isJust ftxAccountFromId
      -> (  $(L.logError)
         $  "Invariant violation: account not in mapping list: "
         <> show field
         )
        >> pure Nothing
      | otherwise
      -> pure Nothing
    Just btxAccount -> pure $ Just btxAccount

mkBluecoinTransaction
  :: (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> CurrencyCode
  -> HMS.HashMap Text BluecoinAccount
  -> HMS.HashMap Text BluecoinCategory
  -> HMS.HashMap Text FinanciusTag
  -> FxLookup
  -> FinanciusTransaction
  -> MaybeT io BluecoinTransaction
mkBluecoinTransaction conn baseCurrencyCode baccs bcats ftags fxlookup ftx@FinanciusTransaction {..} = do
  let itemName = if T.null ftxNote then "(Unnamed transaction)" else ftxNote
  btxItemId :: RowId <- getOrCreateItem conn itemName
  let btxNotes  = "passyImportId:" <> ftxId
  let btxAmount = ftxAmount * 10000
  let btxDate   = PClock.posixSecondsToUTCTime . realToFrac $ ftxDate `div` 1000
  let btxLabels :: [Text] =
        ftagName <$> catMaybes (flip HMS.lookup ftags <$> ftxTagIds)
  let btxConversionRate  = ftxExchangeRate
  let btxTransactionType = getBtxType ftx
  let dailyFxRate = backtrackFxRate fxlookup (FxDate $ Clock.utctDay btxDate)

  btxAccount' <- getBtxAccount baccs ftx
  let taggedAccount :: TransactionBundle (BluecoinAccount, Maybe FxRecord) = flip (,) dailyFxRate <$> btxAccount'
  btxAccount :: TransactionBundle (BluecoinAccount, Double) <- traverse (mergeAccountRates baseCurrencyCode) taggedAccount

  btxCategoryId <- MaybeT . pure $ getBtxCategoryId bcats ftx

  return BluecoinTransaction {..}

mergeAccountRates
  :: L.MonadLogger m
  => CurrencyCode
  -> (BluecoinAccount, Maybe FxRecord)
  -> m (BluecoinAccount, Double)
mergeAccountRates baseCurrencyCode (bacc, mfxr) = do
  let rate = mfxr >>= fxRateForCurrency baseCurrencyCode bacc
  case rate of
    Just r -> return (bacc, r)
    Nothing -> do
      $(L.logWarn) $ "Lookup of fx rate for account " <> show bacc <> " failed."
      return (bacc, 1.0)

-- | This is rather silly. A Map with header values would be much better.
fxRateForCurrency
  :: CurrencyCode
  -> BluecoinAccount
  -> FxRecord
  -> Maybe Double
fxRateForCurrency (CurrencyCode baseCurrencyCode) BluecoinAccount{baccCurrencyCode} FxRecord{..} = do
  -- TODO: Don't hardcode base rate.
  baseRate <- HMS.lookup baseCurrencyCode fxRates
  targetRate <- if baccCurrencyCode == "EUR" then Just 1.0 else HMS.lookup baccCurrencyCode fxRates

  return $ targetRate / baseRate

getBtxType :: FinanciusTransaction -> BluecoinTransactionType
getBtxType FinanciusTransaction {..}
  | ftxNote == "Account balance update" && ftxCategoryId == empty
  = BtxNewAccount
  | otherwise
  = ftxToBtxType ftxTransactionType

getBtxCategoryId
  :: HMS.HashMap Text BluecoinCategory -> FinanciusTransaction -> Maybe RowId
getBtxCategoryId bcats FinanciusTransaction {..}
  | ftxNote == "Account balance update" = return $ bcatId newAccountCategory
  | otherwise = do
    cat <- ((`HMS.lookup`bcats) =<< ftxCategoryId) <|> Just transferCategory
    return $ bcatId cat

mkBluecoinAccount :: AccountMapping -> FinanciusAccount -> Maybe BluecoinAccount
mkBluecoinAccount accountMapping FinanciusAccount {..} = do
  let baccCurrencyCode = faccCurrencyCode
  let baccName         = faccName
  baccId <- RowId <$> HMS.lookup faccId accountMapping
  pure BluecoinAccount {..}

writeBluecoinTransaction
  :: forall (io :: * -> *)
   . (MonadIO io, L.MonadLogger io)
  => SQL.Connection
  -> BluecoinTransaction
  -> io ()
writeBluecoinTransaction conn btx@BluecoinTransaction {..} = do
  let amount = case btxTransactionType of
        BtxExpense    -> negate btxAmount
        BtxIncome     -> btxAmount
        BtxNewAccount -> btxAmount
        BtxTransfer   -> 0

  txIds <- case btxAccount of
    Single account@(_, fxr) -> do
      -- For foreign transactions, bluecoins does not save the amount transacted
      -- but instead stores it AS AN IEEE-754 MULTIPLICATION WITH THE EXCHANGE RATE.
      -- Yeah, I'm not happy about this.
      txId <- write (round $ fromIntegral amount / fxr) account account
      setTxPairId conn txId txId
      return [txId]
    Double srcAccount@(_, srcFx) destAccount@(_, destFx) -> if btxTransactionType /= BtxTransfer
      then do
        -- I know this is a very lazy way of handling this invariant.
        $(L.logError)
          $  "Invalid Double entry transaction with non-transfer type: "
          <> show btx
        return []
      else do
        srcTxId  <- write (negate . round $ fromIntegral btxAmount / srcFx) srcAccount destAccount
        destTxId <- write (round $ fromIntegral btxAmount * btxConversionRate / destFx) destAccount srcAccount
        setTxPairId conn srcTxId destTxId
        setTxPairId conn destTxId srcTxId
        return [srcTxId, destTxId]

  -- I'm a bit disappointed in myself. There's clearly a way nicer way
  -- for doing this that I can't think of right now.
  forM_ btxLabels $ \label -> forM_ txIds (writeBluecoinLabel conn label)
 where
  write
    :: Integer
    -> (BluecoinAccount, Double)
    -> (BluecoinAccount, Double)
    -> io RowId
  write amount (srcAccount, srcFx) (destAccount, _) = do
    let
      sql :: SQL.Query
        = "INSERT INTO TRANSACTIONSTABLE (itemID, amount, notes, accountID, transactionCurrency, conversionRateNew, transactionTypeID, categoryID, tags, accountReference, accountPairID, uidPairID, deletedTransaction, hasPhoto, labelCount, date)\
            \ VALUES\
            \ (:itemID, :amount, :notes, :accountID, :transactionCurrency, :conversionRateNew, :transactionTypeID, :categoryID, :tags, :accountReference, :accountPairID, :uidPairID, :deletedTransaction, :hasPhoto, :labelCount, :date)"
    liftIO $ SQL.executeNamed
      conn
      sql
      [ ":itemID" := btxItemId
      , ":amount" := amount
      , ":notes" := btxNotes
      , ":date" := btxDate
      , ":categoryID" := btxCategoryId
      , ":hasPhoto" := (0 :: Int)
      , ":labelCount" := length btxLabels
      , ":conversionRateNew" := srcFx
      , ":transactionTypeID" := fromEnum btxTransactionType
      , ":uidPairID" := (-1 :: Int)
      , ":accountID" := baccId srcAccount
      , ":accountPairID" := baccId destAccount
      , ":transactionCurrency" := baccCurrencyCode srcAccount
      , ":accountReference" := (3 :: Int)
      , ":deletedTransaction" := (6 :: Int)
      -- TODO: Is this relevant?
      , ":tags" := ("temptags" :: Text)
      ]
    liftIO $ RowId <$> SQL.lastInsertRowId conn

writeBluecoinLabel
  :: (MonadIO io, L.MonadLogger io) => SQL.Connection -> Text -> RowId -> io ()
writeBluecoinLabel conn label (RowId rowId) = do
  let
    sql :: SQL.Query
      = "INSERT INTO LABELSTABLE (labelName, transactionIDLabels)\
        \ VALUES\
        \ (:label, :rowId)"
  liftIO $ SQL.executeNamed conn sql [":label" := label, ":rowId" := rowId]

setTxPairId
  :: (MonadIO io, L.MonadLogger io) => SQL.Connection -> RowId -> RowId -> io ()
setTxPairId conn txId pairId = do
  let
    sql :: SQL.Query
      = "UPDATE TRANSACTIONSTABLE SET uidPairID = :pairId WHERE transactionsTableID = :txId"
  liftIO $ SQL.executeNamed conn sql [":txId" := txId, ":pairId" := pairId]
