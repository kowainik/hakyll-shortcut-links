{- |
Copyright:  (c) 2019-2020 Kowainik
License:    MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module implements parser of the shortcut links that are introduced in the
[shortcut-links](https://hackage.haskell.org/package/shortcut-links) package.

The markdown example of the shortcut link:

@
[This project]\(\@github(Kowainik):hakyll-shortcut-links)
@

The implemented parser should parse the @url@ part of the link.
In this example the parsed links would look like this:

>>> parseShortcut "@github(Kowainik):hakyll-shortcut-links"
Right ("github", Just "Kowainik", Just "hakyll-shortcut-links")

-}

module Hakyll.ShortcutLinks.Parser
       ( parseShortcut
       ) where

import Data.Text (Text)
import Text.Parsec (Parsec, anyChar, many1, noneOf, optionMaybe, parse, (<|>))
import Text.Parsec.Char (alphaNum, char)

import qualified Data.Text as T


type Parser = Parsec Text ()

{- | Parses a shortcut link. Allowed formats:

@
\@name
\@name:text
\@name(option)
\@name(option):text
@
-}
parseShortcut :: Text -> Either String (Text, Maybe Text, Maybe Text)
parseShortcut = either (Left . show) Right . parse p ""
  where
    name :: Parser Text
    name = T.pack <$> many1 (alphaNum <|> char '-')

    option, text :: Parser Text
    option = fmap T.pack $ char '(' *> many1 (noneOf [')']) <* char ')'
    text   = fmap T.pack $ char ':' *> many1 anyChar

    p :: Parser (Text, Maybe Text, Maybe Text)
    p = do
        _ <- char '@'
        (,,) <$> name
             <*> optionMaybe option
             <*> optionMaybe text
