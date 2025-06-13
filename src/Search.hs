-- | Search a name or date
module Search where

import Process
import Data.Char(toLower)
import Data.List (isInfixOf)
import OrthodoxEaster
import Data.Time
import GHC.Num

-- Function to find nameday dates for a specific (case-insensitive)name
searchByName :: String -> Root -> [String]
searchByName searchName root =
  let entries = data_ root
      searchNameLower = map toLower searchName
      matchingEntries = filter (hasName searchNameLower) entries
  in map date matchingEntries
  where
    hasName :: String -> Entry -> Bool
    hasName nameToFind entry =
      any (\n -> nameToFind `isInfixOf` map toLower n) (name entry)

-- Function to find nameday dates for a specific (case-insensitive)name that is affected by the data of easter
searchByNameEaster :: String -> RootEaster -> Integer -> [String]
searchByNameEaster searchName root year =
  let entries = special root
      searchNameLower = map toLower searchName
      matchingEntries = filter (hasName searchNameLower) entries
      easter = orthodoxEaster year
  in map (\entry -> show $ addDays (integerFromInt $ toEaster entry) easter ) matchingEntries
  where
    hasName :: String -> EntryEaster-> Bool
    hasName nameToFind entry =
      any (\n -> nameToFind `isInfixOf` map toLower n) (variations entry)
