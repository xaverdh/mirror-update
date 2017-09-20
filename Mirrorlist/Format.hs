module Mirrorlist.Format where

import Data.Char

{-
  Data definitions for Arch mirrorlist.
  -}

data Mirrorlist =
  Mirrorlist {
    header :: [String],
    countryBlocks :: [CountryBlock]
  } deriving (Eq,Ord,Show)

data CountryBlock =
  CountryBlock {
    name :: String,
    servers :: [Url]
  } deriving (Eq,Ord,Show)

newtype Url = Url {
    getUrl :: String
  } deriving (Eq,Ord,Show)


{-
  Funtions for working with Arch mirrorlist.
  -}

weakEq :: String -> String -> Bool
weakEq s1 s2 = weaken s1 == weaken s2
  where weaken = map toLower . filter (not . isSpace)

filterCountries :: [String] -> Mirrorlist -> Mirrorlist
filterCountries countries mlist =
  mlist { countryBlocks = newBlocks }
  where
    newBlocks = filter select (countryBlocks mlist)
    select block = any ( weakEq (name block) ) countries


extractUrls :: Mirrorlist -> [Url]
extractUrls mlist = do
  blk <- countryBlocks mlist
  url <- servers blk
  pure url




