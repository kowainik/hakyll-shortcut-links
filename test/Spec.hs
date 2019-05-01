module Main (main) where

import Data.Text (Text)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.Markdown (writeMarkdown)

import Hakyll.Shortcut.Links (applyAllShortcuts)


main :: IO ()
main =
    runIOorExplode (readMarkdown def testMd)
    >>= \pandoc -> case applyAllShortcuts pandoc of
        Left errs       -> mapM_ putStrLn errs
        Right newPandoc -> runIOorExplode (writeMarkdown def newPandoc) >>= print

testMd :: Text
testMd = "This is [Hello](@github:kowainik) yes no [aelve/shortcut-links](@github)"
