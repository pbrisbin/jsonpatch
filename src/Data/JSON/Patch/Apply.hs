module Data.JSON.Patch.Apply
  ( applyPatches
  ) where

import Prelude

import Control.Lens ((%=), (.=))
import Control.Monad.Except (MonadError, runExcept)
import Control.Monad.State (MonadState, execStateT, put)
import Data.Aeson (Value)
import Data.Aeson.Lens (_Array)
import Data.Foldable (traverse_)
import Data.JSON.Patch.Type
import Data.JSON.Pointer

applyPatches :: [Patch] -> Value -> Either String Value
applyPatches ps v = runExcept $ flip execStateT v $ traverse_ applyPatch ps

applyPatch
  :: (MonadError String m, MonadState Value m)
  => Patch
  -> m ()
applyPatch = \case
  Add AddOp {path, value} -> case path of
    PointerEmpty -> put value
    PointerPath ts t -> tokensL ts . tokenL t .= value
    PointerPathEnd ts -> tokensL ts . _Array %= (<> pure value)
