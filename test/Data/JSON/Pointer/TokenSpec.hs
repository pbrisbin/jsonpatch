module Data.JSON.Pointer.TokenSpec
  ( spec
  ) where

import Prelude

import Data.Foldable (for_)
import Data.JSON.Pointer.Token
import Test.Hspec

spec :: Spec
spec = do
  fdescribe "tokenFromText" $ do
    it "parses numbers" $ do
      tokenFromText "0" `shouldBe` Right (N 0)

    it "parses end-of-array" $ do
      tokenFromText "-" `shouldBe` Right E

    it "parses object keys" $ do
      tokenFromText "foo" `shouldBe` Right (K "foo")

    it "unescapes ~0 and ~1" $ do
      tokenFromText "fo~0o~1bar" `shouldBe` Right (K "fo~o/bar")

    it "round-trips with tokenToText" $ do
      let ts =
            [ N 0
            , E
            , K "foo"
            , K "fo~o/bar"
            ]

      for_ ts $ \t -> do
        tokenFromText (tokenToText t) `shouldBe` Right t
