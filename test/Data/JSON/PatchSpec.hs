module Data.JSON.PatchSpec
  ( spec
  ) where

import Prelude

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.JSON.Patch
import Data.List (sort)
import Path
import Path.IO
import Test.Hspec
import Test.Hspec.Expectations.Json

spec :: Spec
spec = do
  context "fixtures" $ do
    (dirs, _) <- runIO $ listDirRel fixtures

    for_ (sort dirs) $ \dir -> specify (toFilePath dir) $ do
      (document, patch, expected) <- decodeFixtures dir

      case applyPatches patch document of
        Left err -> expectationFailure $ "Patches failed: " <> err
        Right actual -> actual `shouldMatchJson` expected

fixtures :: Path Rel Dir
fixtures = [reldir|examples|]

decodeFixtures :: HasCallStack => Path Rel Dir -> IO (Value, [Patch], Value)
decodeFixtures dir =
  (,,)
    <$> decodeFixture (fixtures </> dir </> [relfile|input.json|])
    <*> decodeFixture (fixtures </> dir </> [relfile|patch.json|])
    <*> decodeFixture (fixtures </> dir </> [relfile|output.json|])

decodeFixture :: (FromJSON a, HasCallStack) => Path Rel File -> IO a
decodeFixture file = do
  bytes <- BSL.readFile fp
  case eitherDecode bytes of
    Left ex -> do
      expectationFailure $ "Invalid fixture: " <> fp <> ":\n" <> ex
      error "unreachable"
    Right a -> pure a
 where
  fp = toFilePath file
