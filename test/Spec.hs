module Main (main) where

import Test.Hspec (hspec)

import Test.Links (linksSpec)
import Test.Parser (parserSpecs)


main :: IO ()
main = hspec $ do
    parserSpecs
    linksSpec
