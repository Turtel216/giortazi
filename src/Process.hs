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

-- | Read and parse the JSON file containing namedays from recurring namedays
readJSON :: IO Root
readJSON = do
  content <- B.readFile "./data/recurring_namedays.json"
  let decoded = eitherDecode content :: Either String Root
  case decoded of
    Left err -> error $ "Error parsing JSON: " ++ err
    Right root -> return root

-- | Single entry in the easter JSON data set
data EntryEaster = EntryEaster
  { toEaster :: Int
  , main :: String
  , variations :: [String]
  }deriving (Show)

instance FromJSON EntryEaster where
  parseJSON = withObject "EntryEaster" $ \v -> EntryEaster
    <$> v .: "toEaster"
    <*> v .: "main"
    <*> v .: "variations"

-- | Top level easter JSON entry
data RootEaster = RootEaster
 { special :: [EntryEaster]
 } deriving (Show)

instance FromJSON RootEaster where
  parseJSON = withObject "Root" $ \v -> RootEaster
    <$> v .: "special"

-- | Read and parse the JSON file containing namedays from relative to easter dates
readEasterJSON :: IO RootEaster
readEasterJSON = do
  content <- B.readFile "./data/relative_to_easter.json"
  let decoded = eitherDecode content :: Either String RootEaster
  case decoded of
    Left err -> error $ "Error parsing JSON: " ++ err
    Right root -> return root
