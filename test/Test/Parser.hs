module Test.Parser
       ( parserSpecs
       ) where

import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hakyll.Shortcut.Links.Parser (parseShortcut)


parserSpecs :: Spec
parserSpecs = describe "links parsing" $ do
    it "can parse name" $
        parseShortcut "@github" `shouldBe`
            Right ("github", Nothing, Nothing)
    it "can parse name and option" $
        parseShortcut "@github(kowainik)" `shouldBe`
            Right ("github", Just "kowainik", Nothing)
    it "can parse name,option and text" $
        parseShortcut "@github(kowainik):hakyll-shortcut-links" `shouldBe`
            Right ("github", Just "kowainik", Just "hakyll-shortcut-links")
    it "can parse name and text" $
        parseShortcut "@github:hakyll-shortcut-links" `shouldBe`
            Right ("github", Nothing, Just "hakyll-shortcut-links")
    it "doesn't parse link without @" $
        parseShortcut "github" `shouldSatisfy` isLeft
    it "doesn't parse not closed bracket in option" $
        parseShortcut "@github(kowainik" `shouldSatisfy` isLeft
    it "parse until valid symbol" $ do
        parseShortcut "@github kowainik" `shouldBe` Right ("github", Nothing, Nothing)
        parseShortcut "@github)" `shouldBe` Right ("github", Nothing, Nothing)
    it "doesn't parse empty text" $
        parseShortcut "@github:" `shouldSatisfy` isLeft
