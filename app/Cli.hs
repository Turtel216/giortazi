{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parse and handle command line arguments for the giortazi CLI tool
module Cli where

import Options.Applicative
import GHC.Generics (Generic)
import Search as S
import Process as P
import Utils (getCurrentYear, concurrently)

-- | Command data type for CLI options
data GiortaziCommand
  = Today
  | ByName String
  | ByDate String
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

-- Parser for `name` command
nameParser :: Parser GiortaziCommand
nameParser = ByName <$> argument str
  ( metavar "NAME"
 <> help "Name to look up" )

-- Parser for `date` command
dateParser :: Parser GiortaziCommand
dateParser = ByDate <$> argument str
  ( metavar "DATE"
 <> help "Date in format DD-MM" )

-- Entry point TODO: this is temp
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

searchNameEaster :: String -> IO [String]
searchNameEaster name = do
  easterDataset <- P.readEasterJSON
  year <- getCurrentYear
  return $ S.searchByNameEaster name easterDataset year

searchNameNormal :: String -> IO [String]
searchNameNormal name = do
  dataset <- P.readJSON
  return $ S.searchByName name dataset

searchDateNormal :: String -> IO [String]
searchDateNormal date = do
  dataset <- P.readJSON
  return $ S.searchByDate date dataset

searchDateEaster :: String -> IO [String]
searchDateEaster date = do
  year <- getCurrentYear
  dataset <- P.readEasterJSON
  return $ S.searchByDateEaster date year dataset

-- | Command handlers
runCommand :: GiortaziCommand -> IO ()
runCommand Today = undefined -- TODO: Implement today command
runCommand (ByName name) = do
  (normal, easter) <- concurrently (searchNameNormal name) (searchNameEaster name)
  putStrLn $ "Namedays for " ++ name ++ ":"
  mapM_ putStrLn $ normal ++ easter
runCommand (ByDate date) = do -- TODO: Fix formatting of names. They display without spaces
  (normal, easter) <- concurrently (searchDateNormal date) (searchDateEaster date)
  putStrLn $ "Namedays for date " ++ date ++ ":"
  mapM_ putStrLn $ normal ++ easter
