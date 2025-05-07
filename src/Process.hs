{-# LANGUAGE OverloadedStrings #-}

-- | Process the nameday dataset
module Process where

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- | Single entry in the JSON data set
data Entry = Entry
  { name :: [String]
  , date :: String
  }deriving (Show)

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \v -> Entry
    <$> v .: "names"
    <*> v .: "date"

-- | Top level JSON entry
data Root = Root
 { data_ :: [Entry]
 } deriving (Show)

instance FromJSON Root where
  parseJSON = withObject "Root" $ \v -> Root
    <$> v .: "data"

readJSON :: IO Root
readJSON = do
  content <- B.readFile "./data/recurring_namedays.json"
  let decoded = eitherDecode content :: Either String Root
  case decoded of
    Left err -> error $ "Error parsing JSON: " ++ err
    Right root -> return root
