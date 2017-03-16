module Main where

import MirrorUpdate

import Control.Monad.Reader


main :: IO ()
main = getConf
  >>= runReaderT updateMirrorlist

