-- |
--
-- Module      : Data.Vector.ExtSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
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
    let input = V.fromList "abcd"

    it "deletes an element, shifting later elements left" $ do
      V.deleteAt 0 input `shouldBe` V.fromList "bcd"
      V.deleteAt 1 input `shouldBe` V.fromList "acd"
      V.deleteAt 2 input `shouldBe` V.fromList "abd"
      V.deleteAt 3 input `shouldBe` V.fromList "abc"

    it "ignores index out of bound" $ do
      V.deleteAt (-1) input `shouldBe` input
      V.deleteAt 4 input `shouldBe` input

  describe "insertAt" $ do
    let input = V.fromList "abc"

    it "inserts an element, shifting later elements right" $ do
      V.insertAt 0 'x' input `shouldBe` V.fromList "xabc"
      V.insertAt 1 'x' input `shouldBe` V.fromList "axbc"
      V.insertAt 2 'x' input `shouldBe` V.fromList "abxc"
      V.insertAt 3 'x' input `shouldBe` V.fromList "abcx"

    it "ignores index out of bound" $ do
      V.insertAt (-1) 'x' input `shouldBe` input
      V.insertAt 4 'x' input `shouldBe` input
