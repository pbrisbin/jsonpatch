module Data.Vector.ExtSpec
  ( spec
  ) where

import Prelude

import Data.Vector qualified as V
import Data.Vector.Ext qualified as V
import Test.Hspec

spec :: Spec
spec = do
  describe "deleteAt" $ do
    it "deletes an element, shifting later elements left" $ do
      let
        input = V.fromList ['a', 'b', 'c', 'd']
        expected = V.fromList ['a', 'c', 'd']

      V.deleteAt 1 input `shouldBe` expected

    it "ignores index out of bound" $ do
      let input = V.fromList ['a', 'b', 'c', 'd']
      V.deleteAt (-1) input `shouldBe` input
      V.deleteAt 4 input `shouldBe` input
      V.deleteAt 7 input `shouldBe` input

  describe "insertAt" $ do
    it "inserts an element, shifting later elements right" $ do
      let
        input = V.fromList ['a', 'c', 'd']
        expected = V.fromList ['a', 'b', 'c', 'd']

      V.insertAt 1 'b' input `shouldBe` expected

    it "inserts an element at the end by using last index" $ do
      let
        input = V.fromList ['a', 'b', 'c']
        expected = V.fromList ['a', 'b', 'c', 'd']

      V.insertAt 3 'd' input `shouldBe` expected

    it "ignores index out of bound" $ do
      let input = V.fromList ['a', 'c', 'd']
      V.insertAt (-1) 'b' input `shouldBe` input
      V.insertAt 4 'd' input `shouldBe` input
      V.insertAt 7 'b' input `shouldBe` input

  describe "setAt" $ do
    it "sets an element at the given index" $ do
      let
        input = V.fromList ['a', 'b', 'c', 'd']
        expected = V.fromList ['a', 'x', 'c', 'd']

      V.setAt 1 'x' input `shouldBe` expected

    it "ignores index out of bound" $ do
      let input = V.fromList ['a', 'b', 'c', 'd']
      V.setAt (-1) 'x' input `shouldBe` input
      V.setAt 4 'x' input `shouldBe` input
      V.setAt 7 'x' input `shouldBe` input
