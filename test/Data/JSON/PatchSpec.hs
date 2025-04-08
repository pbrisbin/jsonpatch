module Data.JSON.PatchSpec
  ( spec
  ) where

import Prelude

import Data.Aeson (eitherDecode)
import Data.Aeson.QQ
import Data.JSON.Patch
import Data.Text.Lazy.Encoding (encodeUtf8)
import Test.Hspec
import Test.Hspec.Expectations.Json
import Text.Shakespeare.Text (lt)

spec :: Spec
spec = do
  context "Simple example" $ do
    it "works" $ do
      document <-
        either failure pure
          $ eitherDecode
          $ encodeUtf8
            [lt|{
              "baz": "qux",
              "foo": "bar"
            }|]

      patches <-
        either failure pure
          $ eitherDecode
          $ encodeUtf8
            [lt|[
              { "op": "replace", "path": "/baz", "value": "boo" },
              { "op": "add", "path": "/hello", "value": ["world"] },
              { "op": "remove", "path": "/foo" }
            ]|]

      actual <- either failure pure $ applyPatches patches document
      actual `shouldMatchJson` [aesonQQ|{ baz: "boo", hello: ["world"] }|]

failure :: HasCallStack => String -> IO a
failure msg = do
  expectationFailure msg
  error "unreachable"
