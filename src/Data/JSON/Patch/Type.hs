module Data.JSON.Patch.Type
  ( Patch (..)
  , AddOp (..)
  , RemoveOp (..)
  , ReplaceOp (..)
  , CopyOp (..)
  , MoveOp (..)
  , TestOp (..)
  ) where

import Prelude

import Data.Aeson
import Data.JSON.Pointer
import Data.Text (Text)
import GHC.Generics (Generic)

data Patch
  = Add AddOp
  | Remove RemoveOp
  | Replace ReplaceOp
  | Copy CopyOp
  | Move MoveOp
  | Test TestOp

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
      _ -> fail "TODO"

data AddOp = AddOp
  { path :: Pointer
  , value :: Value
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype RemoveOp = RemoveOp
  { path :: Pointer
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ReplaceOp = ReplaceOp
  { path :: Pointer
  , value :: Value
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data CopyOp = CopyOp
  { from :: Pointer
  , path :: Pointer
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data MoveOp = MoveOp
  { from :: Pointer
  , path :: Pointer
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data TestOp = TestOp
  { path :: Pointer
  , value :: Value
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
