module Mirrorlist.Format where

import Data.Char

{-
  Data definitions for Arch mirrorlist.
  -}

data Mirrorlist =
  Mirrorlist {
    header :: [String],
    countryBlocks :: [CountryBlock]
  } deriving (Eq,Show)

data CountryBlock =
  CountryBlock {
    name :: String,
    servers :: [Url]
  } deriving (Eq,Show)

newtype Url = Url {
    getUrl :: String
  } deriving (Show,Eq)


{-
  Funtions for working with Arch mirrorlist.
  -}

weakEq :: String -> String -> Bool
weakEq s1 s2 = weaken s1 == weaken s2
  where weaken = map toLower . filter (not . isSpace)

filterCountries :: [String] -> Mirrorlist -> Mirrorlist
filterCountries countries mlist = 
  let
    select block = foldr ((||) . weakEq (name block)) False countries
    newBlocks = filter select (countryBlocks mlist)
  in mlist { countryBlocks = newBlocks }

extractUrls :: Mirrorlist -> [Url]
extractUrls mlist = do
  blk <- countryBlocks mlist
  url <- servers blk
  return url




