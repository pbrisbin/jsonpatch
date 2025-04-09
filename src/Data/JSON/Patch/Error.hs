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

import Prelude

import Control.Exception (Exception (..))
import Data.Aeson (Value)
import Data.JSON.Pointer
import Data.JSON.Pointer.Token
import Data.Vector (Vector)
import Data.Vector qualified as V

data PatchError
  = ParseError Value String
  | PointerNotFound [Token] (Maybe String)
  | InvalidObjectOperation [Token] Value
  | InvalidArrayOperation [Token] Value
  | IndexOutOfBounds [Token] Int (Vector Value)
  | EmptyArray [Token]
  | TestFailed Pointer Value Value
  deriving stock (Show)

instance Exception PatchError where
  displayException = \case
    ParseError v msg ->
      "Unable to parse Patch(es) from Value: "
        <> ("\n  error: " <> msg)
        <> ("\n  input: " <> show v)
    PointerNotFound ts mType ->
      "Path "
        <> tokensToString ts
        <> " does not exist"
        <> maybe "" (" or is not " <>) mType
    InvalidObjectOperation ts v ->
      "Cannot perform object operation on non-object at "
        <> tokensToString ts
        <> ": "
        <> show v
    InvalidArrayOperation ts v ->
      "Cannot perform array operation on non-array at "
        <> tokensToString ts
        <> ": "
        <> show v
    IndexOutOfBounds ts n vec ->
      "Index "
        <> show n
        <> " is out of bounds for vector of length "
        <> show (V.length vec)
        <> " at "
        <> tokensToString ts
    EmptyArray ts ->
      "Cannot perform operation on empty array at " <> tokensToString ts
    TestFailed p actual expected ->
      "Test failed at "
        <> pointerToString p
        <> ("\n  expected: " <> show expected)
        <> ("\n    actual: " <> show actual)
