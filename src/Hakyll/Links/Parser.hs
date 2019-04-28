module Hakyll.Links.Parser
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
    shortcut = some (alphaNumChar <|> char '-')
    opt      = char '(' *> some (noneOf [')']) <* char ')'
    text     = char ':' *> some anySingle

    p :: Parser (Text, Maybe Text, Maybe Text)
    p = do
        _ <- char '@'
        (,,) . T.pack <$> shortcut
             <*> optional (T.pack <$> opt)
             <*> optional (T.pack <$> text)
