module Test.Links
       ( linksSpec
       ) where

import Control.Monad.Except (throwError)
import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Error (PandocError (PandocSomeError))
import Text.Pandoc.Options (WrapOption (..), WriterOptions (..), def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.Markdown (writeMarkdown)

import Hakyll.ShortcutLinks (applyAllShortcuts)

import qualified Data.Text as T


linksSpec :: Spec
linksSpec = describe "shortcuts expanding" $ do
    it "expands single valid shortcut" $
        expandShortcuts md1Before `shouldBe` Right md1After
    it "expands multiple mixed links" $
        expandShortcuts md2Before `shouldBe` Right md2After
    it "fails on invalid shortcut" $ do
        expandShortcuts mdError1 `shouldSatisfy` isLeft
        expandShortcuts mdError2 `shouldSatisfy` isLeft
    it "fails on multiple words without text" $ do
        expandShortcuts mdError3 `shouldSatisfy` isLeft
    it "idempotence: doesn't not touch content if it doesn't have shorcuts" $ do
        expandShortcuts md1After `shouldBe` Right md1After
        expandShortcuts md2After `shouldBe` Right md2After

{- | Helper function that feeds markdown text to pandoc, applies shortcuts and
returns either error or new text.
-}
expandShortcuts :: Text -> Either String Text
expandShortcuts txt = first show $ runPure $ do
    md <- readMarkdown def txt
    case applyAllShortcuts md of
        Left errs   -> throwError $ PandocSomeError $ T.pack $ unlines errs
        Right newMd -> writeMarkdown def{ writerWrapText = WrapPreserve } newMd

md1Before, md1After :: Text
md1Before = "This is [Hello](@github:kowainik)"
md1After  = "This is [Hello](https://github.com/kowainik)\n"

md2Before, md2After :: Text
md2Before = T.unlines
    [ "Link 1: [Hello World](@github:kowainik)"
    , "Link 2: [Ordinary link](https://stackoverflow.com/)"
    , "Link 3: [kowainik/hakyll-shortcut-links](@github)"
    , "Link 4: [summoner](@hackage). Check text."
    ]
md2After = T.unlines
    [ "Link 1: [Hello World](https://github.com/kowainik)"
    , "Link 2: [Ordinary link](https://stackoverflow.com/)"
    , "Link 3: [kowainik/hakyll-shortcut-links](https://github.com/kowainik/hakyll-shortcut-links)"
    , "Link 4: [summoner](https://hackage.haskell.org/package/summoner). Check text."
    ]

mdError1, mdError2, mdError3 :: Text
mdError1 = "This is invalid [shortcut](@unknown)"
mdError2 = "Another invalid [`shortcut`](@github)"
mdError3 = "Strange link [many words](@github)"
