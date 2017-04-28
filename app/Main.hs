{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Protolude
import Options.Generic (ParseRecord, getRecord)

data Args = Args
  { financiusFile :: FilePath
  , bluecoinFile :: FilePath
  } deriving (Eq, Show, Generic, ParseRecord)

main :: IO ()
main = do
  args :: Args <- getRecord "financius2bluecoin"
  print args
