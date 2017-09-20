module Main where

import MirrorUpdate

import Control.Monad.Reader
import Options.Applicative.Builder
import Options.Applicative.Extra
import Options.Applicative

import System.IO
import System.FilePath
import System.Exit
import System.Environment (getArgs,getProgName)
import Data.Semigroup

main :: IO ()
main = do
  args <- getArgs
  case execParserPure prefs parserInfo args of
    Success r -> runReaderT updateMirrorlist r
    Failure f -> hPutStrLn stderr $ fst $ renderFailure f "mirror-update" 
    CompletionInvoked compl -> do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
      exitSuccess
  where
    parserInfo = info (parser <**> helper) mempty
    prefs = defaultPrefs
      { prefShowHelpOnEmpty = True
      , prefMultiSuffix = "..."}

parser :: Parser Config
parser = pure mkConfig
  <*> strOption ( short 'd' <> long "directory"
    <> value mldir <> showDefault <> metavar "DIRECTORY"
    <> help "The directory where the mirror list is stored." )
  <*> strOption ( short 'b' <> long "backup"
    <> value bakPath <> showDefault <> metavar "BACKUP_PATH"
    <> help "Where to store the backup." )
  <*> some countryArg
  where
    countryArg = strArgument
      $ metavar "COUNTRY" <> help "allowed countries"
    mldir = "/etc/pacman.d"
    bakPath = mldir </> "mirrorlist" <.> "backup"


