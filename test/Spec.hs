module Main (main) where

import Data.Text (Text)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.Markdown (writeMarkdown)

import Hakyll.Links (applyAllShortcuts)


main :: IO ()
main =
    runIOorExplode (readMarkdown def testMd) >>= runIOorExplode . writeMarkdown def . applyAllShortcuts >>= print

testMd :: Text
testMd = "This is [Hello](@github:kowainik) yes no [aelve/shortcut-links](@github)"
