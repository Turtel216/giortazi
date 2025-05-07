{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Process CLI flags
module Cli where

import Options.Applicative
import Search as S
import Process as P

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

-- | Search function
search :: Options -> IO ()
search Options{..} = do
  dataset <- P.readJSON
  let date = S.searchByName name dataset
  print date
