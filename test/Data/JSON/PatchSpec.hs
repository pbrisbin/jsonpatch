module Data.JSON.PatchSpec
  ( spec
  ) where

import Prelude

import Data.Aeson (Value, eitherDecode)
import Data.Aeson.QQ
import Data.JSON.Patch
import Data.Text.Lazy.Encoding (encodeUtf8)
import Test.Hspec
import Text.Shakespeare.Text (lt)

spec :: Spec
spec = do
  context "Simple example" $ do
    it "works" $ do
      let
        documentT =
          [lt|
            {
              "baz": "qux",
              "foo": "bar"
            }
          |]

        patchesT =
          [lt|
            [
              { "op": "replace", "path": "/baz", "value": "boo" },
              { "op": "add", "path": "/hello", "value": ["world"] },
              { "op": "remove", "path": "/foo" }
            ]
          |]

        expected =
          [aesonQQ|
            {
              "baz": "boo",
              "hello": ["world"]
            }
          |]

      let actual = do
            document <- eitherDecode @Value $ encodeUtf8 documentT
            patches <- eitherDecode @[Patch] $ encodeUtf8 patchesT
            applyPatches patches document

      actual `shouldBe` Right expected
