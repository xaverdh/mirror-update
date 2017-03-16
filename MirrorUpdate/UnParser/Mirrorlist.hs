module UnParser.Mirrorlist where

import Format.Mirrorlist
import Data.List (intercalate)

unparse :: String -> String -> Mirrorlist -> String
unparse countryPrefix serverPrefix (Mirrorlist {
    header = header,
    countryBlocks = blks
  }) = intercalate "\n"
    $ header ++ map unparseBlk blks
  where
    unparseBlk :: CountryBlock -> String
    unparseBlk (CountryBlock {
        name = name,
        servers = servers
      }) = intercalate "\n"
        $ (countryPrefix ++ name) : map unparseServer servers
    
    unparseServer :: Url -> String
    unparseServer = ((serverPrefix ++ "Server = ")++) . getUrl


toStrings :: String -> String -> Mirrorlist -> [String]
toStrings countryPrefix serverPrefix (Mirrorlist {
    header = header,
    countryBlocks = blks
  }) = header ++ (blks >>= unparseBlk)
  where
    unparseBlk :: CountryBlock -> [String]
    unparseBlk (CountryBlock {
        name = name,
        servers = servers
      }) = (countryPrefix ++ name) : map unparseServer servers
    
    unparseServer :: Url -> String
    unparseServer = ((serverPrefix ++ "Server = ")++) . getUrl
