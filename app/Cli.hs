{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parse and handle command line arguments for the giortazi CLI tool
module Cli where

import Options.Applicative
import GHC.Generics (Generic)
import Search as S
import Process as P
import Utils (getCurrentYear, concurrently, getCurrentDate, intToDay)

-- | Command data type for CLI options
data GiortaziCommand
  = Today -- ^ Command to show today's name days
  | ByName String -- ^ Command to look up name day by name
  | ByDate String -- ^ Command to look up name day by date
  deriving (Show, Generic)

-- | Parser for each subcommand
giortaziParser :: Parser GiortaziCommand
giortaziParser = hsubparser
  ( command "today" (info (pure Today)
      (progDesc "Show today's name days"))
 <> command "name" (info nameParser
      (progDesc "Lookup name day by name"))
 <> command "date" (info dateParser
      (progDesc "Lookup name day by date"))
  )

-- | Parser for `name` command
nameParser :: Parser GiortaziCommand
nameParser = ByName <$> argument str
  ( metavar "NAME"
 <> help "Name to look up" )

-- | Parser for `date` command
dateParser :: Parser GiortaziCommand
dateParser = ByDate <$> argument str
  ( metavar "DATE"
 <> help "Date in format DD-MM" )

-- | Entry point for the CLI application
main' :: IO ()
main' = do
  opts <- execParser optsParserInfo
  runCommand opts

-- | Parser info including help text
optsParserInfo :: ParserInfo GiortaziCommand
optsParserInfo = info (giortaziParser <**> helper)
  ( fullDesc
 <> progDesc "Print a greeting for NAME"
 <> header "giortazi - a CLI tool to look up Orthodox namedays" )

-- | Search by name function for namedays that depend on easter
searchNameEaster :: String -> IO [String]
searchNameEaster name = do
  easterDataset <- P.readEasterJSON
  year <- getCurrentYear
  return $ S.searchByNameEaster name easterDataset year

-- | Search by name functions for namedays that do not depend on easter
searchNameNormal :: String -> IO [String]
searchNameNormal name = do
  dataset <- P.readJSON
  return $ S.searchByName name dataset

-- | Search by date functions for namedays that do not depend on easter
searchDateNormal :: String -> IO [String]
searchDateNormal date = do
  dataset <- P.readJSON
  return $ S.searchByDate date dataset

-- | Search by date functions for namedays that do not depend on easter
searchDateEaster :: String -> IO [String]
searchDateEaster date = do
  year <- getCurrentYear
  dataset <- P.readEasterJSON
  return $ S.searchByDateEaster date year dataset

-- | Command handlers
runCommand :: GiortaziCommand -> IO ()
runCommand Today = do
  (_, month, day) <- getCurrentDate
  let date = intToDay day month
  (normal, easter) <- concurrently (searchDateNormal date) (searchDateEaster date)
  putStrLn $ "Namedays for date " ++ date ++ ":"
  mapM_ putStrLn $ normal ++ easter
runCommand (ByName name) = do
  (normal, easter) <- concurrently (searchNameNormal name) (searchNameEaster name)
  putStrLn $ "Namedays for " ++ name ++ ":"
  mapM_ putStrLn $ normal ++ easter
runCommand (ByDate date) = do
  (normal, easter) <- concurrently (searchDateNormal date) (searchDateEaster date)
  putStrLn $ "Namedays for date " ++ date ++ ":"
  mapM_ putStrLn $ normal ++ easter
