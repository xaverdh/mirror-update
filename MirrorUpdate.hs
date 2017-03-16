{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- GADTs #-}
module MirrorUpdate where

import Mirrorlist.Format
import Mirrorlist.Parser
import Mirrorlist.UnParser

import Text.Parsec (parse)
import Data.List (intercalate)
import Data.Char (isSpace)
import Data.Text (pack)

import System.Environment (getArgs)
import System.Directory
import System.FilePath
import System.IO
import System.Process

import Control.Monad
import Control.Monad.Reader


data Config = 
  Config {
    mlistDir :: FilePath,
    mlistPath :: FilePath,
    pacnewPath :: FilePath,
    backupPath :: FilePath,
    allowedCountries :: [String]
  }

mkDefaultConfig countries =
  let
    dir = "/etc/pacman.d"
  in Config
    dir
    (dir </> "mirrorlist")
    (dir </> "mirrorlist.pacnew")
    (dir </> "mirrorlist.backup")
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

getConf :: IO Config
getConf = do
  countries <- getArgs
  pure $ mkDefaultConfig countries

getMirrors :: ReaderT Config IO String
getMirrors = do
  pacnew <- (liftIO . readFile) =<< asks pacnewPath
  allowed <- asks allowedCountries
  path <- asks mlistPath
  pure
    $ unparse "# " ""
    . filterCountries allowed
    . either (error . show) id
    . parse mirrorlist path
    $ pacnew

rankMirrors :: String -> IO String
rankMirrors mlist = do
  (fp,fh) <- openTempFile "/tmp" "mirror-update"
  hPutStr fh mlist
  hClose fh
  r <- readProcess "/usr/bin/rankmirrors" ["-n","8",fp] ""
  removeFile fp
  pure r



writeMirrors :: String -> ReaderT Config IO ()
writeMirrors = \case
  "" -> error "Refusing to write empty mirror file."
  mirrors -> do
    bp <- asks backupPath
    mp <- asks mlistPath
    liftIO
      $ copyFile mp bp
      >> writeFile mp mirrors  

updateMirrorlist :: ReaderT Config IO ()
updateMirrorlist =
  assertPerms >>= \case
    True ->
      getMirrors
      >>= liftIO . rankMirrors
      >>= writeMirrors 
    False -> error "Insufficient Privileges; are you root ?"


{-
  TODO:
  allow config to be set from calling scipt,
  by passing command line arguments.
  -}



