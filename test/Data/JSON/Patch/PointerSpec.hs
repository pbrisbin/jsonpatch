-- |
--
-- Module      : Data.JSON.Patch.PointerSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.Patch.PointerSpec
  ( spec
  ) where

import Prelude

import Data.JSON.Pointer
import Data.JSON.Pointer.Token
import Test.Hspec

spec :: Spec
spec = do
  describe "pointerFromText" $ do
    context "success" $ do
      it "empty" $ do
        pointerFromText "" `shouldBe` Right (Pointer [])

      it "root-only" $ do
        pointerFromText "/" `shouldBe` Right (Pointer [K ""])

      it "path" $ do
        pointerFromText "/foo/0/bar/1/baz"
          `shouldBe` Right
            ( Pointer
                [ K "foo"
                , N 0
                , K "bar"
                , N 1
                , K "baz"
                ]
            )

      it "path end of array" $ do
        pointerFromText "/foo/0/bar/1/-"
          `shouldBe` Right
            ( Pointer
                [ K "foo"
                , N 0
                , K "bar"
                , N 1
                , E
                ]
            )

      it "handles implementation-specific numeric parsing" $ do
        pointerFromText "/1e0" `shouldBe` Right (Pointer [K "1e0"])
