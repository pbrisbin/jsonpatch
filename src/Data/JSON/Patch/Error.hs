module Data.JSON.Patch.Error
  ( PatchError (..)
  ) where

import Prelude

import Control.Exception (Exception (..))
import Data.Aeson (Value)
import Data.JSON.Pointer
import Data.JSON.Pointer.Token
import Data.Vector (Vector)

data PatchError
  = ParseError Value String
  | TestFailed Pointer Value Value
  | PointerNotFound [Token] (Maybe String)
  | InvalidObjectOperation [Token] Value
  | IndexOutOfBounds [Token] Int (Vector Value)
  | EmptyArray [Token]
  deriving stock (Show)
  deriving anyclass (Exception)
