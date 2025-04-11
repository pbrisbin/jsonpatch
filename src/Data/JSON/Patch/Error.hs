-- |
--
-- Module      : Data.JSON.Patch.Error
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.Patch.Error
  ( PatchError (..)
  ) where

import Data.JSON.Patch.Prelude

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.Char8 as BSL8
import Data.JSON.Pointer
import Data.Vector qualified as V

data PatchError
  = ParseError Value String
  | PointerNotFound Value Pointer
  | InvalidObjectOperation Pointer Value
  | InvalidArrayOperation Pointer Value
  | IndexOutOfBounds Pointer Int (Vector Value)
  | EmptyArray Pointer
  | TestFailed Pointer Value Value
  deriving stock (Show)

instance Exception PatchError where
  displayException = \case
    ParseError v msg ->
      "Unable to parse Patch(es) from Value: "
        <> ("\n  error: " <> msg)
        <> ("\n  input: " <> showValue v)
    PointerNotFound v ts ->
      "Path "
        <> pointerToString ts
        <> " does not exist in "
        <> showValue v
    InvalidObjectOperation ts v ->
      "Cannot perform object operation on non-object at "
        <> pointerToString ts
        <> ": "
        <> showValue v
    InvalidArrayOperation ts v ->
      "Cannot perform array operation on non-array at "
        <> pointerToString ts
        <> ": "
        <> showValue v
    IndexOutOfBounds ts n vec ->
      "Index "
        <> show n
        <> " is out of bounds for vector of length "
        <> show (V.length vec)
        <> " at "
        <> pointerToString ts
    EmptyArray ts ->
      "Cannot perform operation on empty array at " <> pointerToString ts
    TestFailed p actual expected ->
      "Test failed at "
        <> pointerToString p
        <> ("\n  expected: " <> showValue expected)
        <> ("\n    actual: " <> showValue actual)

showValue :: Value -> String
showValue = BSL8.unpack . encode
