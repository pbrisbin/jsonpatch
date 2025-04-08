module Data.JSON.PointerSpec
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
