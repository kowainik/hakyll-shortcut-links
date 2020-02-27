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
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, noneOf, optional, parse, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char)

import qualified Data.Text as T


type Parser = Parsec Void Text

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
    name = T.pack <$> some (alphaNumChar <|> char '-')

    option, text :: Parser Text
    option = fmap T.pack $ char '(' *> some (noneOf [')']) <* char ')'
    text   = fmap T.pack $ char ':' *> some anySingle

    p :: Parser (Text, Maybe Text, Maybe Text)
    p = do
        _ <- char '@'
        (,,) <$> name
             <*> optional option
             <*> optional text
