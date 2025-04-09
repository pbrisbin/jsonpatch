# jsonpatch

[![Hackage](https://img.shields.io/hackage/v/jsonpatch.svg?style=flat)](https://hackage.haskell.org/package/jsonpatch)
[![Stackage Nightly](http://stackage.org/package/jsonpatch/badge/nightly)](http://stackage.org/nightly/package/jsonpatch)
[![Stackage LTS](http://stackage.org/package/jsonpatch/badge/lts)](http://stackage.org/lts/package/jsonpatch)
[![CI](https://github.com/pbrisbin/jsonpatch/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/jsonpatch/actions/workflows/ci.yml)

Haskell package for parsing and applying [JSON Patches][jsonpatch].

[jsonpatch]: https://jsonpatch.com/

## Example

Typical use cases need only one import:

<!--

```haskell
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where
import Prelude
import Text.Markdown.Unlit ()
```

-->

```haskell
import Data.Aeson (Result(..), fromJSON)
import Data.Aeson.Encode.Pretty
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Lazy qualified as BSL
import Data.JSON.Patch

main :: IO ()
main = do
  let
    Success patch = fromJSON [aesonQQ|
      [
        { "op": "replace", "path": "/baz", "value": "boo" },
        { "op": "add", "path": "/hello", "value": ["world"] },
        { "op": "remove", "path": "/foo" }
      ]
    |]

    document = [aesonQQ|
      {
        "baz": "qux",
        "foo": "bar"
      }
    |]


  either fail (BSL.putStr . encodePretty) $ applyPatches patch document
```

The above program outputs:

```json
{
  "baz": "boo",
  "hello": ["world"]
}
```

The result is in `Either String`, with error messages returned as `Left` values.

## Quality

The full test suite from [`json-patch/json-patch-tests`][json-patch-tests]
passes. However, some error cases have poor (or misleading) error messages at
this time.

[json-patch-tests]: https://github.com/json-patch/json-patch-tests

## License

This package is licensed AGPLv3. See [COPYING](./COPYING).
