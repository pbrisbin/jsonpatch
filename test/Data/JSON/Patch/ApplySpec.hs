module Data.JSON.Patch.ApplySpec
  ( spec
  ) where

import Prelude

import Data.Aeson (Result (..), Value, fromJSON)
import Data.Aeson.QQ
import Data.JSON.Patch.Apply
import Test.Hspec
import Test.Hspec.Expectations.Json

spec :: Spec
spec = do
  describe "applyPatches" $ do
    context "Add" $ do
      it "to end-of-array" $ do
        actual <-
          applyPatches'
            [aesonQQ|[ { op: "add", path: "/foo/-", value: "bat" } ]|]
            [aesonQQ|{ foo: ["bar", "baz"] }|]

        actual `shouldMatchJson` [aesonQQ|{ foo: ["bar", "baz", "bat"] }|]

      it "in the middle of an array" $ do
        actual <-
          applyPatches'
            [aesonQQ|[ { op: "add", path: "/foo/2", value: "bat" } ]|]
            [aesonQQ|{ foo: ["bar", "baz", "bit"] }|]

        actual `shouldMatchJson` [aesonQQ|{ foo: ["bar", "baz", "bat", "bit"] }|]

    context "Remove" $ do
      it "from end-of-array" $ do
        actual <-
          applyPatches'
            [aesonQQ|[ { op: "remove", path: "/foo/-" } ]|]
            [aesonQQ|{ foo: ["bar", "baz"] }|]

        actual `shouldMatchJson` [aesonQQ|{ foo: ["bar"] }|]

      it "in the middle of an array" $ do
        actual <-
          applyPatches'
            [aesonQQ|[ { op: "remove", path: "/foo/1" } ]|]
            [aesonQQ|{ foo: ["bar", "baz", "bit"] }|]

        actual `shouldMatchJson` [aesonQQ|{ foo: ["bar", "bit"] }|]

applyPatches' :: Value -> Value -> IO Value
applyPatches' patchesV document = do
  patches <- case fromJSON patchesV of
    Error err -> failure err
    Success ps -> pure ps

  either failure pure $ applyPatches patches document

failure :: HasCallStack => String -> IO a
failure msg = do
  expectationFailure msg
  error "unreachable"
