# hakyll-shortcut-links

![scissors](https://user-images.githubusercontent.com/4276606/56942330-9bf93d80-6b4c-11e9-8286-3cb98eb0c94f.png)
[![Hackage](https://img.shields.io/hackage/v/hakyll-shortcut-links.svg?logo=haskell)](https://hackage.haskell.org/package/hakyll-shortcut-links)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Build status](https://img.shields.io/travis/kowainik/hakyll-shortcut-links.svg?logo=travis)](https://travis-ci.org/kowainik/hakyll-shortcut-links)

This library adds the support of the
[`shortcut-links`](https://hackage.haskell.org/package/shortcut-links) library
in the static website generator library —
[`Hakyll`](https://hackage.haskell.org/package/hakyll).

That being said, it means that you can use shortcuts from `shortcut-links`
library or custom ones in your markdown files which would be extended into
complete URLs during the Hakyll site compilation process.

Here is the example of how links could look like before expansion:

```
Here is going to be a link to the [hakyll-shortcut-links](@hackage) library on Hackage.
And another link to the [GitHub sources](@github(kowainik):hakyll-shortcut-links).
```

which is going to be transformed into the ordinary links automatically by the
library functions:

```
Here is going to be a link to the [hakyll-shortcut-links](http://hackage.haskell.org/package/hakyll-shortcut-links) library on Hackage.
And another link to the [GitHub sources](https://github.com/kowainik/hakyll-shortcut-links).
```
For the full list of the supported shortcuts, you can check
[ShortcutLinks.All module](https://hackage.haskell.org/package/shortcut-links/docs/ShortcutLinks-All.html)
of the `shortcut-links` library.

## How to use in a Hakyll project

To use `hakyll-shortcut-links` in your ready Hakyll project, you would need to
go through the following steps:

### Add the dependency

Add `hakyll-shortcut-links` under the `build-depends` section in your `.cabal`
file.
If you use `Stack` build tool, you would need to add the library into
`extra-deps` list in the `stack.yaml` file.

### Change a standard pandocCompiler

Here we assume that you already have a Hakyll project. If not then you can use
[these tutorials](https://jaspervdj.be/hakyll/tutorials.html) to create one.
Anyway, you would eventually have the code that uses `pandocCompiler` that could
look like this function:

```haskell
compile $
    pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx
```

All you need to change is to use `allShortcutLinksCompiler` or
`shortcutLinksCompiler` function from this library instead of the standard
`pandocCompiler`:

```haskell
import Hakyll.ShortcutLinks (allShortcutLinksCompiler)

...

compile $
    allShortcutLinksCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx
```

That's all!

There could be another situation. You could already have some custom Compiler
function:

```haskell
-- | A pandoc compiler which makes all entries of "42" bold automatically.
bold42Compiler :: Compiler (Item String)
bold42Compiler = pandocCompilerWithTransform
    myHakyllReaderOptions
    myHakyllWriterOptions
    make42Bold
```

In this case, you can use the `applyAllShortcuts` function directly. All you
need is to combine two transformations:

```haskell
myCompiler :: Compiler (Item String)
myCompiler = pandocCompilerWithTransformM
    myHakyllReaderOptions
    myHakyllWriterOptions
    (applyAllShortcuts . make42Bold)
```

### Use shortcuts in your docs

The hardest part is done, now you can create links using the shortcuts, and the
Hakyll is going to build full URLs for them. Happy coding!

### Example

Here is an example of the pull request that introduces the
`hakyll-shortcut-links` library into the scope:

* [Some Hakyll website](https://github.com/vrom911/vrom911.github.io/pull/32)

As you can see by the diff the only actual change needed for that was replacing
`pandocCompiler` with `allShortcutLinksCompiler`.

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/) is licensed by [CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
