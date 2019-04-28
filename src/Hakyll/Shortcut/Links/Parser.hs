module Hakyll.Shortcut.Links.Parser
       ( parseShortcut
       ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, noneOf, optional, parse, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char)

import qualified Data.Text as T


type Parser = Parsec Void Text

{- | Parse a shortcut link. Allowed formats:

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
