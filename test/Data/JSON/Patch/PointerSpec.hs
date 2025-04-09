module Data.JSON.Patch.PointerSpec
  ( spec
  ) where

import Prelude

import Data.JSON.Pointer
import Test.Hspec

spec :: Spec
spec = do
  describe "pointerFromText" $ do
    context "success" $ do
      it "empty" $ do
        pointerFromText "" `shouldBe` Right PointerEmpty

      it "root-only" $ do
        pointerFromText "/" `shouldBe` Right (PointerPath [] $ K "")

      it "path" $ do
        pointerFromText "/foo/0/bar/1/baz"
          `shouldBe` Right
            ( PointerPath
                [ K "foo"
                , N 0
                , K "bar"
                , N 1
                ]
                $ K "baz"
            )

      it "path end of array" $ do
        pointerFromText "/foo/0/bar/1/-"
          `shouldBe` Right
            ( PointerPathEnd
                [ K "foo"
                , N 0
                , K "bar"
                , N 1
                ]
            )
