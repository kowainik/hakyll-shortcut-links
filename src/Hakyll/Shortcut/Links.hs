module Hakyll.Shortcut.Links
       ( applyShortcuts
       , applyAllShortcuts
       , shortcutLinksCompiler
       , allShortcutLinksCompiler
       ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Hakyll (Compiler, Item, defaultHakyllReaderOptions, defaultHakyllWriterOptions,
               pandocCompilerWithTransform)
import ShortcutLinks (Result (..), Shortcut, allShortcuts, useShortcutFrom)
import Text.Pandoc.Generic (bottomUp)

import Hakyll.Shortcut.Links.Parser (parseShortcut)

import qualified Data.Text as T
import qualified Text.Pandoc.Definition as Pandoc


{- | Modifies links to add an extra anchor which links to the header.
-}
applyShortcuts :: [([Text], Shortcut)] -> Pandoc.Pandoc -> Pandoc.Pandoc
applyShortcuts shortcuts = bottomUp applyLink
  where
    applyLink :: Pandoc.Inline -> Pandoc.Inline
    applyLink l@(Pandoc.Link attr inl (url, title)) = case parseShortcut $ T.pack url of
        Right (name, option, text) ->
            case useShortcutFrom shortcuts name option $ fromMaybe (checkTitle inl) text of
                Success link   -> Pandoc.Link attr inl (T.unpack link, title)
                Warning _ link -> Pandoc.Link attr inl (T.unpack link, title)
                Failure _      -> l
        Left _ -> l
    applyLink other = other

    checkTitle :: [Pandoc.Inline] -> Text
    checkTitle = \case
        [] -> error "invalid shortcut link title arguments"
        [Pandoc.Str s] -> T.pack s
        _ -> error "Invalid shortcut links title arguments"

{- | Modifies links to add an extra anchor which links to the header.
-}
applyAllShortcuts :: Pandoc.Pandoc -> Pandoc.Pandoc
applyAllShortcuts = applyShortcuts allShortcuts

-- | Our own pandoc compiler which parses shortcut links automatically.
shortcutLinksCompiler :: [([Text], Shortcut)] -> Compiler (Item String)
shortcutLinksCompiler = pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    . applyShortcuts

-- | Our own pandoc compiler which parses shortcut links automatically.
allShortcutLinksCompiler :: Compiler (Item String)
allShortcutLinksCompiler = shortcutLinksCompiler allShortcuts
