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
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
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

runPatchTests :: Path b File -> Spec
runPatchTests path = do
  tests <- runIO $ decodeFileThrow @[PatchTest] path
  zipWithM_ runPatchTest [0 ..] tests

runPatchTest :: HasCallStack => Int -> PatchTest -> Spec
runPatchTest n t = do
  let it' = if t.focus then fit else it

  it' ("#" <> show n <> " " <> t.comment) $ do
    when t.disabled $ pendingWith "disabled"

    case fromJSON t.patch of
      Error err ->
        case t.expected of
          -- Failed and should have
          Left e -> validateErrors err e
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
          (Left ex, Left e) -> validateErrors ex e

validateErrors :: String -> String -> IO ()
validateErrors actual expected = do
  case lookup expected errorsMap of
    Nothing ->
      expectationFailure
        $ unlines
          [ "Error case not seen before"
          , "Add the following (or similar) as a new element to errorsMap:"
          , ", ( " <> show expected <> ", (== " <> show actual <> "))"
          ]
    Just p
      | not $ p actual ->
          expectationFailure
            $ unlines
              [ "Error " <> show expected <> " did not pass predicate"
              , "Actual message: " <> show actual
              ]
    _ -> pure ()

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

errorsMap :: [(String, String -> Bool)]
errorsMap =
  [ ("JSON Pointer should start with a slash", (== "'/': Failed reading: satisfy"))
  , ("Object operation on array target", (== "/-: Object operation on non-object"))
  , ("Out of bounds (upper)", ("is out of bounds" `isInfixOf`))
  , ("Unrecognized op 'spam'", ("unexpected operation" `isPrefixOf`))
  , ("add to a non-existent target", (== "/baz/-: the specified value doesn't exist"))
  , ("copy op shouldn't work with bad number", (== "/baz/-: the specified value does not exist"))
  , ("index is greater than number of items in array", ("is out of bounds" `isInfixOf`))
  , ("missing 'from' location", (== "/-: the specified value does not exist"))
  , ("missing 'from' parameter", ("key \"from\" not found" `isSuffixOf`))
  , ("missing 'path' parameter", ("key \"path\" not found" `isSuffixOf`))
  , ("missing 'value' parameter", ("key \"value\" not found" `isSuffixOf`))
  , ("move op shouldn't work with bad number", (== "/baz/-: the specified value does not exist"))
  , ("null is not valid value for 'path'", ("encountered Null" `isSuffixOf`))
  , ("number is not equal to string", (== "/~1/-: test failed: Number 10.0 != String \"10\""))
  , ("path /a does not exist -- missing objects are not created recursively", (== "/a/-: the specified value doesn't exist"))
  , ("remove op shouldn't remove from array with bad number", ("the specified value doesn't exist" `isSuffixOf`))
  , ("removing a nonexistent field should fail", ("the specified value doesn't exist" `isSuffixOf`))
  , ("removing a nonexistent index should fail", ("the specified value doesn't exist" `isSuffixOf`))
  , ("replace op should fail with missing parent key", (== "/foo/bar/-: the specified value doesn't exist"))
  , ("replace op shouldn't replace in array with bad number", (== "/1e0/-: the specified value doesn't exist"))
  , ("string not equivalent", (== "/baz/-: test failed: String \"qux\" != String \"bar\""))
  , ("test op should fail", (== "/foo/-: test failed: Object (fromList [(\"bar\",Array [Number 1.0,Number 2.0,Number 5.0,Number 4.0])]) != Array [Number 1.0,Number 2.0]"))
  , ("test op shouldn't get array element 1", (== "/-: the specified value does not exist"))
  ]
  <> todo
 where
  -- These need fixing or to be improved
  todo =
    [ ("Out of bounds (lower)", (== "/bar/-: Object operation on non-object"))
    , ("add op shouldn't add to array with bad number", (== "/-: Object operation on non-object"))
    , ("test op should reject the array value, it has leading zeros", (== "endOfInput"))
    , ("test op should reject the array value, it has leading zeros", (== "endOfInput"))
    ]
