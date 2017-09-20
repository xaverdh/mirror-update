{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MirrorUpdate where

import Mirrorlist.Format
import Mirrorlist.Parser
import Mirrorlist.UnParser

import Data.Attoparsec.ByteString (parseOnly,endOfInput)
import qualified Data.ByteString as BS

import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

import Control.Monad
import Control.Monad.Reader

parseMirrorlist :: BS.ByteString -> Either String Mirrorlist
parseMirrorlist = parseOnly (mirrorlist <* endOfInput)

instance HaveParserConfig where
  countryPrefix = "# "  
  serverPrefix = ""

data Config = 
  Config {
    mlistDir :: FilePath,
    mlistPath :: FilePath,
    pacnewPath :: FilePath,
    backupPath :: FilePath,
    allowedCountries :: [String]
  }


mkConfig :: FilePath -> FilePath -> [String] -> Config
mkConfig mldir bakPath countries = Config
  mldir
  ( mldir </> "mirrorlist" )
  ( mldir </> "mirrorlist" <.> "pacnew" )
  bakPath
  countries

assertPerms :: ReaderT Config IO Bool
assertPerms = do
  conf <- ask
  bs <- liftIO $ forM (paths conf) canAccess
  pure $ all id bs
  where
    paths conf = map ($conf) [mlistDir,mlistPath,pacnewPath,backupPath]
    canAccess p = do
      perms <- getPermissions p
      pure $ readable perms && writable perms


getMirrors :: ReaderT Config IO String
getMirrors = do
  pacnew <- (liftIO . BS.readFile) =<< asks pacnewPath
  allowed <- asks allowedCountries
  path <- asks mlistPath
  pure
    $ unparse
    . filterCountries allowed
    . either (error . show) id
    . parseMirrorlist
    $ pacnew

rankMirrors :: String -> IO String
rankMirrors mlist = do
  withSystemTempFile "mirror-update" $ \fp fh -> do
    hPutStr fh mlist
    hClose fh
    readProcess "/usr/bin/rankmirrors" ["-n","8",fp] ""


writeMirrors :: String -> ReaderT Config IO ()
writeMirrors = \case
  "" -> error "Refusing to write empty mirror file."
  mirrors -> do
    bp <- asks backupPath
    mp <- asks mlistPath
    liftIO
      $ copyFile mp bp
      *> writeFile mp mirrors  

updateMirrorlist :: ReaderT Config IO ()
updateMirrorlist =
  assertPerms >>= \case
    True ->
      getMirrors
      >>= liftIO . rankMirrors
      >>= writeMirrors 
    False -> error "Insufficient Privileges; are you root ?"


