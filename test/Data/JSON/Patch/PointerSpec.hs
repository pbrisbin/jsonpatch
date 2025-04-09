module Data.JSON.Patch.PointerSpec
  ( spec
  ) where

import Prelude

import Data.Aeson (Value (..))
import Data.Aeson.QQ
import Data.JSON.Pointer
import Optics
import Test.Hspec

spec :: Spec
spec = do
  describe "atTokenL" $ do
    it "deleting nested within arrays" $ do
      let
        input = [aesonQQ|{ "baz": [{"boo": "net" }] }|]
        optic = tokensL [K "baz", N 0] % atTokenL (K "boo")
        updated = input & optic .~ Nothing

      updated `shouldBe` [aesonQQ|{ "baz": [{}] }|]
