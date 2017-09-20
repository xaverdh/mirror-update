module Mirrorlist.Parser where

{-
  Parsing of Arch mirrorlists.
  Tries to be liberal in what it accepts as input.
  -}

import Mirrorlist.Format

import Text.Parser.Char hiding (space,spaces)
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Char
import Data.Functor
import Control.Applicative


mirrorlist :: (Monad m,CharParsing m,LookAheadParsing m)
  => m Mirrorlist
mirrorlist = Mirrorlist
  <$> mirrorlistHeader
  <*> (blocks <* eof)
  where
    blocks = many $ discard *> try countryBlock <* discard
    discard = emptyLines

mirrorlistHeader :: (CharParsing m,LookAheadParsing m)
  => m [String]
mirrorlistHeader =
  try (manyTill anyChar eol)
  `manyTill` try (lookAhead countryBlock)

countryBlock :: CharParsing m => m CountryBlock
countryBlock = CountryBlock
  <$> country
  <*> some (try server)


country :: CharParsing m => m String
country = 
  prefix *> manyTill anyChar eol <?> "<country name>"
  where
    prefix = skipMany (char '#') *> spaces

server :: CharParsing m => m Url
server = prefix *> url <?> "Server = <server url>"
  where
    prefix =
      skipMany ( char '#' )
      *> spaces
      *> string "Server"
      *> spaces
      *> char '='
      *> spaces $> ()

url :: CharParsing m => m Url
url = Url
  <$> ( pure "http" <++> secure
        <++> pure "://" <++> manyTill anyChar eol )
  where
    (<++>) = liftA2 (<>)
    secure :: CharParsing m => m String
    secure = (char 's' $> "s") <|> pure ""

eol :: CharParsing m => m ()
eol = skipOptional (char '\r') *> newline $> () <?> "end of line"

space :: CharParsing m => m ()
space = satisfy (
    (&&) <$> isSpace <*> (not . isControl)
  ) $> () <?> "space"

spaces :: CharParsing m => m ()
spaces = skipMany space

emptyLine :: CharParsing m => m ()
emptyLine =
  skipOptional (char '#')
  *> skipOptional (char '#')
  *> spaces
  *> eol <?> "empty line"

emptyLines :: CharParsing m => m ()
emptyLines = skipMany $ try emptyLine





