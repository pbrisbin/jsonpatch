-- |
--
-- Module      : Data.JSON.Patch.Type
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- <https://datatracker.ietf.org/doc/html/rfc6902>
module Data.JSON.Patch.Type
  ( Patch (..)
  , AddOp (..)
  , RemoveOp (..)
  , ReplaceOp (..)
  , CopyOp (..)
  , MoveOp (..)
  , TestOp (..)
  ) where

import Data.JSON.Patch.Prelude

import Data.Aeson
import Data.JSON.Pointer

data Patch
  = Add AddOp
  | Remove RemoveOp
  | Replace ReplaceOp
  | Copy CopyOp
  | Move MoveOp
  | Test TestOp
  deriving stock (Eq, Show)

instance FromJSON Patch where
  parseJSON = withObject "Operation" $ \o -> do
    op <- o .: "op"

    case (op :: Text) of
      "add" -> Add <$> parseJSON (Object o)
      "remove" -> Remove <$> parseJSON (Object o)
      "replace" -> Replace <$> parseJSON (Object o)
      "copy" -> Copy <$> parseJSON (Object o)
      "move" -> Move <$> parseJSON (Object o)
      "test" -> Test <$> parseJSON (Object o)
      x ->
        fail
          $ concat
            [ "unexpected operation, "
            , show x
            , ", must be one of add, remove, replace, copy, move, or test"
            ]

data AddOp = AddOp
  { path :: Pointer
  , value :: Value
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

newtype RemoveOp = RemoveOp
  { path :: Pointer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data ReplaceOp = ReplaceOp
  { path :: Pointer
  , value :: Value
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data CopyOp = CopyOp
  { from :: Pointer
  , path :: Pointer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data MoveOp = MoveOp
  { from :: Pointer
  , path :: Pointer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data TestOp = TestOp
  { path :: Pointer
  , value :: Value
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)
