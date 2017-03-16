module Mirrorlist.Parser where

{-
  Parsing of Arch mirrorlists.
  Tries to be liberal in what it accepts as input.
  -}

import Mirrorlist.Format
import Text.Parsec hiding (space,spaces)
import Data.Maybe (maybeToList)
import Data.Char

mirrorlist :: Parsec String () Mirrorlist
mirrorlist =
  let discard = emptyLines
  in do
    header <- mirrorlistHeader
    blocks <- many $ discard *> try countryBlock <* discard
    eof
    return $ Mirrorlist header blocks

mirrorlistHeader =
  try (manyTill anyChar eol)
  `manyTill` try (lookAhead countryBlock)

countryBlock = do
  name <- country
  urls <- many1 $ try server
  return $ CountryBlock name urls

country =
  let
    prefix = do
      many $ char '#'
      spaces
  in prefix *> manyTill anyChar eol <?> "<country name>"

server = 
  let
    prefix = do
      many $ char '#'
      spaces
      string "Server"
      spaces
      char '='
      spaces
  in prefix *> url <?> "Server = <server url>"

url = do
  string "http"
  s <- optionMaybe $ char 's'
  string "://" 
  rest <- manyTill anyChar eol
  return . Url $ "http" ++ maybeToList s ++ "://" ++ rest

eol = endOfLine <?> "end of line"

space = satisfy (
    (&&) <$> isSpace <*> (not . isControl)
  ) <?> "space"

spaces = many space

emptyLine =
  optional (char '#')
  >> optional (char '#')
  >> many space
  >> eol <?> "empty line"

emptyLines = many $ try emptyLine





