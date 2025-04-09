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

import Control.Exception (displayException)
import Control.Monad (unless, zipWithM_)
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
import Data.JSON.Patch.Error
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
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
  , comment :: Maybe String
  , disabled :: Bool
  , focus :: Bool
  }

instance FromJSON PatchTest where
  parseJSON = withObject "PatchTest" $ \o -> do
    doc <- o .: "doc"
    patch <- o .: "patch"
    expected <- toExpected <$> o .:? "expected" <*> o .:? "error"
    comment <- o .:? "comment"
    disabled <- o .:? "disabled" .!= False
    focus <- o .:? "focus" .!= False
    pure PatchTest {doc, patch, expected, comment, disabled, focus}
   where
    toExpected (Just a) Nothing = Right a
    toExpected Nothing (Just b) = Left b
    toExpected a b = Left $ "invalid test? expected=" <> show a <> ", error=" <> show b

runPatchTests :: Path b File -> Spec
runPatchTests path = do
  tests <- runIO $ decodeFileThrow @[PatchTest] path
  zipWithM_ runPatchTest [0 ..] tests

runPatchTest :: HasCallStack => Int -> PatchTest -> Spec
runPatchTest n t = do
  let
    it'
      | t.disabled = xit
      | t.focus = fit
      | otherwise = it

    comment = fromMaybe ("test #" <> show n) t.comment
    result = case fromJSON t.patch of
      Error err -> Left $ ParseError t.patch err
      Success patches -> applyPatches patches t.doc

  it' comment $ case (result, t.expected) of
    (Left ex, Left e) -> do
      unless (maybe False ($ ex) $ lookup e errorsMap) $ do
        expectationFailure
          $ unlines
            [ "Error " <> show e <> " not known or did not pass predicate"
            , "Actual message: " <> displayException ex
            ]
    (Left ex, Right b) ->
      expectationFailure
        $ unlines
          [ "Error: " <> displayException ex
          , "  Expected:\n" <> indentedPretty 7 b
          , "  Doc:\n" <> indentedPretty 7 t.doc
          , "  Patch:\n" <> indentedPretty 7 t.patch
          ]
    (Right a, Left e) ->
      expectationFailure
        $ unlines
          [ "Expected error: " <> e
          , "Instead got:\n" <> indentedPretty 7 a
          , "  Doc:\n" <> indentedPretty 7 t.doc
          , "  Patch:\n" <> indentedPretty 7 t.patch
          ]
    (Right a, Right b) -> a `shouldMatchJson` b

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

{- FOURMOLU_DISABLE -}

errorsMap :: [(String, PatchError -> Bool)]
errorsMap =
  [ ("JSON Pointer should start with a slash", isParseError)
  , ("Object operation on array target", isInvalidObjectOperation)
  , ("Out of bounds (upper)", isIndexOutOfBounds)
  , ("Unrecognized op 'spam'", isParseError)
  , ("add to a non-existent target", isPointerNotFound)
  , ("copy op shouldn't work with bad number", isPointerNotFound)
  , ("index is greater than number of items in array", isIndexOutOfBounds)
  , ("missing 'from' location", isPointerNotFound)
  , ("missing 'from' parameter", isParseError)
  , ("missing 'path' parameter", isParseError)
  , ("missing 'value' parameter", isParseError)
  , ("move op shouldn't work with bad number", isPointerNotFound)
  , ("null is not valid value for 'path'", isParseError)
  , ("number is not equal to string", isTestFailed)
  , ("path /a does not exist -- missing objects are not created recursively", isPointerNotFound)
  , ("remove op shouldn't remove from array with bad number", isPointerNotFound)
  , ("removing a nonexistent field should fail", isPointerNotFound)
  , ("removing a nonexistent index should fail", isPointerNotFound)
  , ("replace op should fail with missing parent key", isPointerNotFound)
  , ("replace op shouldn't replace in array with bad number", isPointerNotFound)
  , ("string not equivalent", isTestFailed)
  , ("test op should fail", isTestFailed)
  , ("test op shouldn't get array element 1", isPointerNotFound)
  ]
  <> todo
 where
  -- These need fixing or to be improved
  todo =
    [ ("Out of bounds (lower)", isInvalidObjectOperation)
    , ("add op shouldn't add to array with bad number", isInvalidObjectOperation)
    , ("test op should reject the array value, it has leading zeros", isParseError)
    , ("test op should reject the array value, it has leading zeros", isParseError)
    ]

isParseError :: PatchError -> Bool
isParseError = \case
  ParseError{} -> True
  _ -> False

isPointerNotFound :: PatchError -> Bool
isPointerNotFound = \case
  PointerNotFound{} -> True
  _ -> False

isInvalidObjectOperation :: PatchError -> Bool
isInvalidObjectOperation = \case
  InvalidObjectOperation{} -> True
  _ -> False

isIndexOutOfBounds :: PatchError -> Bool
isIndexOutOfBounds = \case
  IndexOutOfBounds{} -> True
  _ -> False

isTestFailed :: PatchError -> Bool
isTestFailed = \case
  TestFailed{} -> True
  _ -> False
