-- |
--
-- Module      : Data.JSON.PatchSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.PatchSpec
  ( spec
  ) where

import Prelude

import Control.Monad (when, zipWithM_)
import Data.Aeson
import Data.Aeson.Encode.Pretty
  ( Config (..)
  , Indent (..)
  , defConfig
  , encodePretty'
  )
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.JSON.Patch
import GHC.Int (Int64)
import Path
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Hspec hiding (focus)
import Test.Hspec.Expectations.Json

data PatchTest = PatchTest
  { doc :: Value
  , patch :: Value
  , expected :: Either String Value
  , comment :: String
  , disabled :: Bool
  , focus :: Bool
  }

instance FromJSON PatchTest where
  parseJSON = withObject "PatchTest" $ \o -> do
    doc <- o .: "doc"
    patch <- o .: "patch"
    expected <- toExpected <$> o .:? "expected" <*> o .:? "error"
    comment <- o .:? "comment" .!= "<no comment>"
    disabled <- o .:? "disabled" .!= False
    focus <- o .:? "focus" .!= False
    pure PatchTest {doc, patch, expected, comment, disabled, focus}
   where
    toExpected (Just a) Nothing = Right a
    toExpected Nothing (Just b) = Left b
    toExpected a b = Left $ "invalid test? expected=" <> show a <> ", error=" <> show b

runPatchTest :: HasCallStack => Int -> PatchTest -> Spec
runPatchTest n t = do
  let it' = if t.focus then fit else it

  it' ("#" <> show n <> " " <> t.comment) $ do
    when t.disabled $ pendingWith "disabled"

    case fromJSON t.patch of
      Error err ->
        case t.expected of
          Left _ -> pure () -- task failed successfully
          Right v ->
            expectationFailure
              $ unlines
                [ "Patch parse error: " <> err
                , "  Expected:\n" <> indentedPretty 7 v
                , "  Doc:\n" <> indentedPretty 7 t.doc
                , "  Patch:\n" <> indentedPretty 7 t.patch
                ]
      Success patches ->
        case (applyPatches patches t.doc, t.expected) of
          -- Applied, verify result
          (Right a, Right b) -> a `shouldMatchJson` b
          -- Applied but shouldn't have
          (Right a, Left e) ->
            expectationFailure
              $ unlines
                [ "Expected error: " <> e
                , "Instead got:\n" <> indentedPretty 7 a
                , "  Doc:\n" <> indentedPretty 7 t.doc
                , "  Patch:\n" <> indentedPretty 7 t.patch
                ]
          -- Failed but should've applied
          (Left ex, Right b) ->
            expectationFailure
              $ unlines
                [ "Error: " <> ex
                , "  Expected:\n" <> indentedPretty 7 b
                , "  Doc:\n" <> indentedPretty 7 t.doc
                , "  Patch:\n" <> indentedPretty 7 t.patch
                ]
          -- Failed and should have
          (Left _, Left _) -> pure ()

runPatchTests :: Path b File -> Spec
runPatchTests path = do
  tests <- runIO $ decodeFileThrow @[PatchTest] path
  zipWithM_ runPatchTest [0 ..] tests

spec :: Spec
spec = do
  context "json-patch-tests main" $ runPatchTests [relfile|tests.json|]
  context "json-patch-tests RFC6092" $ runPatchTests [relfile|spec_tests.json|]

decodeFileThrow :: FromJSON a => Path b File -> IO a
decodeFileThrow file = do
  bytes <- BSL.readFile fp
  case eitherDecode bytes of
    Left ex -> do
      hPutStrLn stderr $ "Invalid JSON: " <> fp <> ":\n" <> ex
      exitFailure
    Right a -> pure a
 where
  fp = toFilePath file

indentedPretty :: ToJSON a => Int64 -> a -> String
indentedPretty n =
  BSL8.unpack
    . BSL8.unlines
    . map (indent <>)
    . BSL8.lines
    . encodePretty' config
 where
  indent = BSL8.replicate n ' '
  config = defConfig {confIndent = Spaces 2}
