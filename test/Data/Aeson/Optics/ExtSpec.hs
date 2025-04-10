-- |
--
-- Module      : Data.Aeson.Optics.ExtSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.Aeson.Optics.ExtSpec
  ( spec
  ) where

import Prelude

import Data.Aeson.Key qualified as Key
import Data.Aeson.Optics
import Data.Aeson.Optics.Ext
import Data.Aeson.QQ
import Data.Foldable (for_)
import Optics
import Test.Hspec

spec :: Spec
spec = do
  describe "atNth" $ do
    it "indexes arrays as expected" $ do
      let input = [aesonQQ| ["a", "b"] |]

      input ^? atNth (-1) % _Just `shouldBe` Nothing
      input ^? atNth 0 % _Just % _String `shouldBe` Just "a"
      input ^? atNth 1 % _Just % _String `shouldBe` Just "b"
      input ^? atNth 2 % _Just `shouldBe` Nothing

    it "adds to arrays by shifting elements right" $ do
      let input = [aesonQQ| ["a", "b"] |]

      input & atNth (-1) ?~ "x" & (`shouldBe` input)
      input & atNth 0 ?~ "x" & (`shouldBe` [aesonQQ| ["x", "a", "b"] |])
      input & atNth 1 ?~ "x" & (`shouldBe` [aesonQQ| ["a", "x", "b"] |])
      input & atNth 2 ?~ "x" & (`shouldBe` [aesonQQ| ["a", "b", "x"] |])
      input & atNth 3 ?~ "x" & (`shouldBe` input)

    it "removes from arrays by shifting elements left" $ do
      let input = [aesonQQ| ["a", "b", "c"] |]

      input & atNth (-1) .~ Nothing & (`shouldBe` input)
      input & atNth 0 .~ Nothing & (`shouldBe` [aesonQQ| ["b", "c"] |])
      input & atNth 1 .~ Nothing & (`shouldBe` [aesonQQ| ["a", "c"] |])
      input & atNth 2 .~ Nothing & (`shouldBe` [aesonQQ| ["a", "b"] |])
      input & atNth 3 .~ Nothing & (`shouldBe` input)

    context "Objects behave like atKey" $ do
      let input = [aesonQQ| {"0":"a", "1":"b", "2":"c"} |]

      for_ [-1 .. 3] $ \n -> do
        let
          k = Key.fromString $ show n
          index l = input ^? l
          add l = input & l ?~ "x"
          remove l = input & l .~ Nothing

        it ("test index " <> show n) $ do
          index (atNth n) `shouldBe` index (atKey k)
          add (atNth n) `shouldBe` add (atKey k)
          remove (atNth n) `shouldBe` remove (atKey k)

  describe "atEnd" $ do
    it "always indexes as Nothing" $ do
      let input = [aesonQQ| ["a", "b"] |]

      input ^? atEnd % _Just `shouldBe` Nothing

    it "allows appending to an array" $ do
      let input = [aesonQQ| ["a", "b"] |]

      input & atEnd ?~ "x" & (`shouldBe` [aesonQQ| ["a", "b", "x"] |])

    it "cannot remove the non-existent index" $ do
      let input = [aesonQQ| ["a", "b"] |]

      input & atEnd .~ Nothing & (`shouldBe` input)

    it "leaves other types alone" $ do
      let input = [aesonQQ| {"a": "b"} |]

      input & atEnd ?~ "x" & (`shouldBe` input)
