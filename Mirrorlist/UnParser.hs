{-# LANGUAGE MultiParamTypeClasses #-}
module Mirrorlist.UnParser where

import Mirrorlist.Format
import Data.List (intercalate)
import Data.Semigroup

class HaveParserConfig where
  countryPrefix :: String
  serverPrefix :: String

class HaveParserConfig => UnParse a where
  unparse :: a -> String

instance HaveParserConfig => UnParse Mirrorlist where
  unparse mlist = 
    intercalate "\n" $ ( header mlist )
    <> ( unparse <$> countryBlocks mlist )

instance HaveParserConfig => UnParse CountryBlock where
  unparse blk =
    intercalate "\n" $ ( countryPrefix <> name blk )
    : ( urlToServer . unparse <$> servers blk )

urlToServer :: HaveParserConfig => String -> String
urlToServer s = serverPrefix <> "Server = " <> s

instance HaveParserConfig => UnParse Url where
  unparse = getUrl



