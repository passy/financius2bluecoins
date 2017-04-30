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


import qualified Data.Aeson as Aeson
import qualified Control.Monad.Logger as L
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as SQL
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

data Args = Args
  { financiusFile :: FilePath
  , bluecoinFile :: FilePath
  , mappingFile :: FilePath
  } deriving (Eq, Show, Generic, ParseRecord)

type AccountMapping = HMS.HashMap Text Text

data BluecoinTransaction = BluecoinTransaction
  { btxItemId :: Integer
  , btxAmount :: Integer
  , btxNotes :: Text
  } deriving (Eq, Show)

instance Aeson.FromJSON FinanciusTransaction where
  parseJSON = Aeson.withObject "transaction" $ \o ->
    FinanciusTransaction
        <$> o .:? "account_from_id"
        <*> o .:? "account_to_id"
        <*> o .: "note"
        <*> o .: "amount"

data FinanciusTransaction = FinanciusTransaction
  { ftxAccountFromId :: Maybe Text
  , ftxAccountToId :: Maybe Text
  , ftxNote :: Text
  , ftxAmount :: Integer
  } deriving (Eq, Show, Generic)


main :: IO ()
main = L.runStderrLoggingT $ do
  $(L.logInfo) "Getting started ..."
  args :: Args <- getRecord "financius2bluecoin"
  financiusJson <- liftIO . readFile $ financiusFile args
  mappingJson <- liftIO . readFile $ mappingFile args
  conn <- liftIO . SQL.open $ bluecoinFile args

  let accountsMap :: Either Text (HMS.HashMap Text Text) =
        case first T.pack . Aeson.eitherDecodeStrict $ TE.encodeUtf8 mappingJson of
          Left e -> error $ "mapping parsing failed: " <> e
          Right a -> a

  let transactions = fromMaybe V.empty $ financiusJson ^? key "transactions" . _Array
  forM_ transactions $ \tx -> do
    case ftxNote <$> Aeson.fromJSON tx of
      Aeson.Success v ->
        liftIO $ SQL.execute conn "INSERT INTO ITEMTABLE (itemName) VALUES (?)" (SQL.Only v)
      Aeson.Error e ->
        $(L.logError) $ "Couldn't parse record: " <> show e

    -- executeNamed "INSERT INTO TRANSACTIONTABLE (amount, itemID, transactionCurrency, conversionRateNew, transactionTypeID, categoryID, accountID, )"

  $(L.logInfo) "Done."
