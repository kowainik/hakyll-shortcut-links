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

import Hakyll.Shortcut.Links (applyAllShortcuts)

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
        Left errs   -> throwError $ PandocSomeError $ unlines errs
        Right newMd -> writeMarkdown def{ writerWrapText = WrapPreserve } newMd

md1Before, md1After :: Text
md1Before = "This is [Hello](@github:kowainik)"
md1After  = "This is [Hello](https://github.com/kowainik)"

md2Before, md2After :: Text
md2Before = T.intercalate "\n"
    [ "Link 1: [Hello](@github:kowainik)"
    , "Link 2: [Ordinary link](https://stackoverflow.com/)"
    , "Link 3: [kowainik/hakyll-shortcut-links](@github)"
    , "Link 4: [summoner](@hackage)"
    ]
md2After = T.intercalate "\n"
    [ "Link 1: [Hello](https://github.com/kowainik)"
    , "Link 2: [Ordinary link](https://stackoverflow.com/)"
    , "Link 3: [kowainik/hakyll-shortcut-links](https://github.com/kowainik/hakyll-shortcut-links)"
    , "Link 4: [summoner](https://hackage.haskell.org/package/summoner)"
    ]

mdError1, mdError2 :: Text
mdError1 = "This is invalid [shortcut](@unknown)"
mdError2 = "Another invalid [`shortcut`](@github)"
