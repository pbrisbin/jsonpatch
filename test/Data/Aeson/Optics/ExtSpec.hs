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

    context "targeting an Object" $ do
      let input = [aesonQQ| {"0":"a", "1":"b", "2":"c"} |]

      for_ [-1 .. 3] $ \n -> do
        let
          k = Key.fromString $ show n
          index l = input ^? l
          add l = input & l ?~ "x"
          remove l = input & l .~ Nothing

        it ("indexes like atKey for " <> show n) $ do
          index (atNth n) `shouldBe` index (atKey k)

        it ("adds like atKey for " <> show n) $ do
          add (atNth n) `shouldBe` add (atKey k)

        it ("removes like atKey for " <> show n) $ do
          remove (atNth n) `shouldBe` remove (atKey k)
