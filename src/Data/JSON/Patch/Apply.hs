-- |
--
-- Module      : Data.JSON.Patch.Apply
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.Patch.Apply
  ( applyPatches
  , PatchError (..)
  ) where

import Prelude

import Control.Monad (unless, void, when)
import Control.Monad.Except (MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, execStateT, gets, modify, put)
import Data.Aeson (Value (..))
import Data.Foldable (traverse_)
import Data.JSON.Patch.Error
import Data.JSON.Patch.Type
import Data.JSON.Pointer
import Data.JSON.Pointer.Token
import Data.Vector qualified as V
import Optics

applyPatches :: [Patch] -> Value -> Either PatchError Value
applyPatches ps = runExcept . execStateT (traverse_ applyPatch ps)

applyPatch :: (MonadError PatchError m, MonadState Value m) => Patch -> m ()
applyPatch = \case
  Add op -> add op.value op.path
  Remove op -> remove op.path
  Replace op -> remove op.path >> add op.value op.path
  Move op -> do
    v <- get op.from
    remove op.from
    add v op.path
  Copy op -> flip add op.path =<< get op.from
  Test op -> do
    v <- get op.path
    unless (v == op.value) $ throwError $ TestFailed op.path v op.value

get :: (MonadError PatchError m, MonadState Value m) => Pointer -> m Value
get p =
  gets (preview $ pointerL p) >>= \case
    Nothing -> throwError $ PointerNotFound p Nothing
    Just v -> pure v

add :: (MonadError PatchError m, MonadState Value m) => Value -> Pointer -> m ()
add v p = case splitPointer p of
  Nothing -> put v
  Just (parent, t) -> do
    validateAdd parent t
    modify $ atPointerL p ?~ v

remove :: (MonadError PatchError m, MonadState Value m) => Pointer -> m ()
remove p = do
  void $ get p
  modify $ atPointerL p .~ Nothing

validateAdd
  :: (MonadError PatchError m, MonadState Value m)
  => Pointer
  -> Token
  -> m ()
validateAdd parent t = do
  target <- get parent

  case (t, target) of
    (_, Object _) -> pure () -- everything works on objects
    (K _, v) -> throwError $ InvalidObjectOperation parent v
    (N n, Array vec) -> do
      when (n < 0) $ throwError $ IndexOutOfBounds parent n vec
      when (n > V.length vec) $ throwError $ IndexOutOfBounds parent n vec
    (N _, v) -> throwError $ InvalidArrayOperation parent v
    _ -> pure ()
