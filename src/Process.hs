{-# LANGUAGE OverloadedStrings #-}

-- | Process the nameday json datasets
module Process where

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- | Path to normal nameday JSON file
-- This file contains namedays that are fixed to a specific date.
normalJSONPath :: String
normalJSONPath = "./data/recurring_namedays.json"

-- | Path to easter dependent nameday JSON file
-- This file contains namedays that are relative to the date of Easter
-- e.g. "Easter Monday" is always the day after Easter which is a variable date.
easterJSONPath :: String
easterJSONPath = "./data/relative_to_easter.json"

-- | Single entry in the JSON data set
data Entry = Entry
  { name :: [String] -- ^ List of names for the nameday
  , date :: String -- ^ Date of the nameday in "DD/MM" format
  }deriving (Show)

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \v -> Entry
    <$> v .: "names"
    <*> v .: "date"

-- | Top level JSON entry
data Root = Root
 { data_ :: [Entry] -- ^ List of nameday entries
  -- Note: 'data' is a reserved keyword in Haskell, so we use 'data_' instead.
  -- This field contains the actual nameday data.
 } deriving (Show)

instance FromJSON Root where
  parseJSON = withObject "Root" $ \v -> Root
    <$> v .: "data"

-- | Read and parse the JSON file containing namedays from recurring namedays
-- see 'Entry' for the structure of the entries.
--
-- This file contains namedays that are fixed to a specific date.
--
-- The functions throws an error if the JSON cannot be parsed.
readJSON :: IO Root
readJSON = do
  content <- B.readFile normalJSONPath
  let decoded = eitherDecode content :: Either String Root
  case decoded of
    Left err -> error $ "Error parsing JSON: " ++ err
    Right root -> return root

-- | Single entry in the easter JSON data set
data EntryEaster = EntryEaster
  { toEaster :: Int -- ^ Number of days relative to Easter
  , main :: String -- ^ Main name for the nameday
  , variations :: [String] -- ^ List of variations for the nameday
  }deriving (Show)

instance FromJSON EntryEaster where
  parseJSON = withObject "EntryEaster" $ \v -> EntryEaster
    <$> v .: "toEaster"
    <*> v .: "main"
    <*> v .: "variations"

-- | Top level easter JSON entry
data RootEaster = RootEaster
 { special :: [EntryEaster] -- ^ List of nameday entries relative to Easter
  -- This field contains the actual nameday data relative to Easter.
  -- Each entry specifies a number of days relative to Easter, a main name,
  -- and variations of that name.
 } deriving (Show)

instance FromJSON RootEaster where
  parseJSON = withObject "Root" $ \v -> RootEaster
    <$> v .: "special"

-- | Read and parse the JSON file containing namedays from relative to easter dates
-- see 'EntryEaster' for the structure of the entries
--
-- This file contains namedays that are relative to the date of Easter.
--
-- The functions throws an error if the JSON cannot be parsed.
readEasterJSON :: IO RootEaster
readEasterJSON = do
  content <- B.readFile easterJSONPath
  let decoded = eitherDecode content :: Either String RootEaster
  case decoded of
    Left err -> error $ "Error parsing JSON: " ++ err
    Right root -> return root
