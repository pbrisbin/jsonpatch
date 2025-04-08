# jsonpatch

[![Hackage](https://img.shields.io/hackage/v/jsonpatch.svg?style=flat)](https://hackage.haskell.org/package/jsonpatch)
[![Stackage Nightly](http://stackage.org/package/jsonpatch/badge/nightly)](http://stackage.org/nightly/package/jsonpatch)
[![Stackage LTS](http://stackage.org/package/jsonpatch/badge/lts)](http://stackage.org/lts/package/jsonpatch)
[![CI](https://github.com/pbrisbin/jsonpatch/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/jsonpatch/actions/workflows/ci.yml)

Haskell package for parsing and applying [JSON Patches][jsonpatch].

[jsonpatch]: https://jsonpatch.com/

## Example

Typical use cases need only one import:

```hs
import Data.JSON.Patch
```

Use `FromJSON` to parse `[Patch]`. For example,

```json
[
  { "op": "replace", "path": "/baz", "value": "boo" },
  { "op": "add", "path": "/hello", "value": ["world"] },
  { "op": "remove", "path": "/foo" }
]
```

Then use `applyPatches` to apply that to a `Value`.

Given `document` is:

```json
{
  "baz": "qux",
  "foo": "bar"
}
```

Then `applyPatches patch document` would be:


```json
{
  "baz": "boo",
  "hello": ["world"]
}
```

The result is in `Either String`, with error messages returned as `Left` values.

## License

This package is licensed AGPLv3. See [COPYING](./COPYING).
