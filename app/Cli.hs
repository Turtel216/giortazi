{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Process CLI flags
module Cli where

import Options.Applicative
import Search as S
import Process as P
import Utils (getCurrentYear, concurrently)

-- | Data structure for CLI options
data Options = Options
  { name   :: !String
  , shout  :: !Bool
  }

-- | Parser for CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "name"
     <> short 'n'
     <> metavar "NAME"
     <> help "Name to search the name day of" )
  <*> switch
      ( long "date" --TODO: Add other options
     <> short 'd'
     <> help "Get the nameday for a specific date" )

-- Entry point TODO: this is temp
main' :: IO ()
main' = do
  opts <- execParser optsParserInfo
  search opts

-- | Parser info including help text
optsParserInfo :: ParserInfo Options
optsParserInfo = info (optionsParser <**> helper)
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

-- | Search function
search :: Options -> IO ()
search Options{..} = do
  (normal, easter) <- concurrently (searchNameNormal name) (searchNameEaster name)
  putStrLn $ "Namedays for " ++ name ++ ":"
  mapM_ putStrLn $ normal ++ easter
