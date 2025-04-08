module Data.JSON.Patch.Apply
  ( applyPatches
  ) where

import Prelude

import Control.Monad.Except (MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, execStateT, get, gets, modify, put)
import Data.Aeson
import Data.Aeson.Optics
import Data.Foldable (for_, traverse_)
import Data.JSON.Patch.Type
import Data.JSON.Pointer
import Debug.Trace
import Optics

applyPatches :: [Patch] -> Value -> Either String Value
applyPatches ps v = runExcept $ flip execStateT v $ traverse_ applyPatch ps

applyPatch
  :: (MonadError String m, MonadState Value m)
  => Patch
  -> m ()
applyPatch = \case
  Add AddOp {path, value} -> case path of
    PointerEmpty -> put value
    PointerPath ts t -> case t of
      K k -> modify $ tokensL ts % _Object % at k ?~ value
    PointerPathEnd ts -> modify $ tokensL ts % _Array %~ (<> pure value)
  Remove RemoveOp {path} -> case path of
    PointerPath ts t -> case t of
      K k -> modify $ tokensL ts % _Object % at k .~ Nothing
  Replace ReplaceOp {path, value} -> case path of
    PointerEmpty -> put value
    PointerPath ts t -> case t of
      K k -> modify $ tokensL ts % _Object % ix k .~ value
