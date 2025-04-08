module Data.JSON.PatchSpec
  ( spec
  ) where

import Prelude

import Data.Aeson (Value, eitherDecode)
import Data.Aeson.QQ
import Data.JSON.Patch
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Test.Hspec
import Test.Hspec.Expectations.Json
import Text.Shakespeare.Text (lt)

spec :: Spec
spec = do
  context "Simple example" $ do
    it "works" $ do
      runExamplePatch
        [lt|{
          "baz": "qux",
          "foo": "bar"
        }|]
        [lt|[
          { "op": "replace", "path": "/baz", "value": "boo" },
          { "op": "add", "path": "/hello", "value": ["world"] },
          { "op": "remove", "path": "/foo" }
        ]|]
        [aesonQQ|{
          "baz": "boo",
          "hello": ["world"]
        }|]

  context "Add" $ do
    it "to end-of-array" $ do
      runExamplePatch
        [lt|{ "foo": ["bar", "baz"] }|]
        [lt|[ { "op": "add", "path": "/foo/-", "value": "bat" } ]|]
        [aesonQQ|{ "foo": ["bar", "baz", "bat"] }|]

  context "Remove" $ do
    it "from end-of-array" $ do
      runExamplePatch
        [lt|{ "foo": ["bar", "baz"] }|]
        [lt|[ { "op": "remove", "path": "/foo/-" } ]|]
        [aesonQQ|{ "foo": ["bar"] }|]

runExamplePatch :: HasCallStack => Text -> Text -> Value -> Expectation
runExamplePatch documentT patchesT expected = do
  document <- fromLeft "decode document" $ eitherDecode $ encodeUtf8 documentT
  patches <- fromLeft "decode patches" $ eitherDecode $ encodeUtf8 patchesT
  actual <- fromLeft "apply" $ applyPatches patches document
  actual `shouldMatchJson` expected

fromLeft :: HasCallStack => String -> Either String a -> IO a
fromLeft act = \case
  Left er -> do
    expectationFailure $ "Failed to " <> act <> ":\n" <> er
    error "unreachable"
  Right a -> pure a
