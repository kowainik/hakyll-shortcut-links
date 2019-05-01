{-# LANGUAGE FlexibleContexts #-}

module Hakyll.Shortcut.Links
       ( applyShortcuts
       , applyAllShortcuts
       , shortcutLinksCompiler
       , allShortcutLinksCompiler
       ) where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Hakyll (Compiler, Item, defaultHakyllReaderOptions, defaultHakyllWriterOptions,
               pandocCompilerWithTransformM)
import ShortcutLinks (Result (..), Shortcut, allShortcuts, useShortcutFrom)
import Text.Pandoc.Generic (bottomUpM)

import Hakyll.Shortcut.Links.Parser (parseShortcut)

import qualified Data.Text as T
import qualified Text.Pandoc.Definition as Pandoc


{- | Modifies links to add an extra anchor which links to the header.
-}
applyShortcuts
    :: forall m . MonadError [String] m
    => [([Text], Shortcut)]  -- ^ Shortcuts
    -> Pandoc.Pandoc         -- ^ Pandoc document that possibly contains shortened links
    -> m Pandoc.Pandoc       -- ^ Result pandoc document with shorcuts expanded
applyShortcuts shortcuts = bottomUpM applyLink
  where
    applyLink :: Pandoc.Inline -> m Pandoc.Inline
    applyLink l@(Pandoc.Link attr inl (url, title)) = case parseShortcut $ T.pack url of
        Right (name, option, text) -> checkTitle inl >>= \txtTitle ->
            case useShortcutFrom shortcuts name option $ fromMaybe txtTitle text of
                Success link -> pure $ Pandoc.Link attr inl (T.unpack link, title)
                Warning ws _ -> throwError ws
                Failure msg  -> throwError [msg]
        Left _ -> pure l  -- the link is not shortcut
    applyLink other = pure other

    checkTitle :: [Pandoc.Inline] -> m Text
    checkTitle = \case
        [] -> throwError ["Empty shortcut link title arguments"]
        [Pandoc.Str s] -> pure $ T.pack s
        _ -> throwError ["Shortcut title is not a single string element"]

{- | Modifies links to add an extra anchor which links to the header.
-}
applyAllShortcuts :: MonadError [String] m => Pandoc.Pandoc -> m Pandoc.Pandoc
applyAllShortcuts = applyShortcuts allShortcuts

-- | Our own pandoc compiler which parses shortcut links automatically.
shortcutLinksCompiler :: [([Text], Shortcut)] -> Compiler (Item String)
shortcutLinksCompiler = pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    . applyShortcuts

-- | Our own pandoc compiler which parses shortcut links automatically.
allShortcutLinksCompiler :: Compiler (Item String)
allShortcutLinksCompiler = shortcutLinksCompiler allShortcuts
