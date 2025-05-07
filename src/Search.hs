-- | Search a name or date
module Search where

import Process
import Data.Char(toLower)
import Data.List (isInfixOf)

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
