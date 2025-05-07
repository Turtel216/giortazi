-- | Search a name or date
module Search where

import Process

-- Pure function to find dates for a specific name
searchByName :: String -> Root -> [String]
searchByName searchName root =
  let entries = data_ root
      matchingEntries = filter (hasName searchName) entries
  in map date matchingEntries
  where
    hasName :: String -> Entry -> Bool
    hasName nameToFind entry = nameToFind `elem` (name entry)
