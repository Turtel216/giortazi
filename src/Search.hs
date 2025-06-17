-- | Module for searching names or dates from the nameday data sets
module Search where

import Process
import Data.Char(toLower)
import Data.List (isInfixOf)
import Utils (orthodoxEaster, convertDate, fromToEasterToDay)
import GHC.Num
import Data.Time
import GHC.Base (eqString)

-- | Function to find nameday dates for a specific (case-insensitive)name
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

-- | Function to find nameday dates for a specific (case-insensitive)name that is affected by the data of easter
searchByNameEaster :: String -> RootEaster -> Integer -> [String]
searchByNameEaster searchName root year =
  let entries = special root
      searchNameLower = map toLower searchName
      matchingEntries = filter (hasName searchNameLower) entries
      easter = orthodoxEaster year
  in map (\entry -> convertDate $ show $ addDays (integerFromInt $ toEaster entry) easter ) matchingEntries
  where
    hasName :: String -> EntryEaster-> Bool
    hasName nameToFind entry =
      any (\n -> nameToFind `isInfixOf` map toLower n) (variations entry)

-- | Function to find nameday dates for a specific (case-insensitive)date
searchByDate :: String -> Root -> [String]
searchByDate searchDate root =
  let entries = data_ root
      matchingEntries = filter (\entry -> hasDate searchDate entry) entries
  in map (\entry -> concat $ (map (\n -> n ++ " ")) (name entry)) matchingEntries
  where
    hasDate :: String -> Entry -> Bool
    hasDate dateToFind entry =
      map toLower dateToFind `isInfixOf` map toLower (date entry)

-- | Function to find nameday dates for a specific date that is affected by the data of easter
searchByDateEaster :: String -> Integer -> RootEaster -> [String]
searchByDateEaster searchDate year root =
  let entries = special root
      matchingEntries = filter (\entry -> hasDate (toEaster entry) searchDate) entries
  in map (\entry -> concat $ (map (\variation -> variation ++ " ")) (variations entry)) matchingEntries
  where
    hasDate :: Int -> String -> Bool
    hasDate entry dateToFind =
      let dateStr = fromToEasterToDay entry year
      in eqString dateStr dateToFind
