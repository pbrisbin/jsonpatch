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
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Optics
import Data.Foldable (traverse_)
import Data.JSON.Patch.Error
import Data.JSON.Patch.Type
import Data.JSON.Pointer
import Data.JSON.Pointer.Token
import Data.Vector (Vector)
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
get = \case
  PointerEmpty -> gets id
  PointerPath ts t -> assertExists $ ts <> [t]
  PointerPathEnd ts -> do
    target <- assertExists ts
    vec <- assertArray ts target
    snd <$> assertUnsnoc ts vec

add :: (MonadError PatchError m, MonadState Value m) => Value -> Pointer -> m ()
add v = \case
  PointerEmpty -> put v
  PointerPath ts t -> do
    target <- assertExists ts

    -- Additional validations based on type of final target
    case t of
      K _ -> void $ assertObject ts target
      N n -> do
        case target of
          Object {} -> pure () -- n will be used as Key, no bounds check
          Array vec -> do
            when (n < 0) $ throwError $ IndexOutOfBounds ts n vec
            when (n > V.length vec) $ throwError $ IndexOutOfBounds ts n vec
          v' -> throwError $ InvalidArrayOperation ts v'

    modify $ tokensL ts % atTokenL t ?~ v
  PointerPathEnd ts -> do
    target <- assertExists ts
    void $ assertArray ts target
    modify $ tokensL ts % _Array %~ (<> pure v)

remove :: (MonadError PatchError m, MonadState Value m) => Pointer -> m ()
remove = \case
  PointerEmpty -> put Null -- NB. unspecified behavior
  PointerPath ts t -> do
    void $ assertExists $ ts <> [t]

    -- NB. odd that the tests don't exercise any additional validation (e.g.
    -- bounds checking) like we saw with add.

    modify $ tokensL ts % atTokenL t .~ Nothing
  PointerPathEnd ts -> do
    target <- assertExists ts
    vec <- assertArray ts target
    (vs, _) <- assertUnsnoc ts vec
    modify $ tokensL ts % _Array .~ vs

assertExists
  :: (MonadError PatchError m, MonadState Value m) => [Token] -> m Value
assertExists ts =
  gets (preview $ tokensL ts) >>= \case
    Nothing -> throwError $ PointerNotFound ts Nothing
    Just v -> pure v

assertObject :: MonadError PatchError m => [Token] -> Value -> m (KeyMap Value)
assertObject ts = \case
  Object o -> pure o
  v -> throwError $ InvalidObjectOperation ts v

assertArray :: MonadError PatchError m => [Token] -> Value -> m (Vector Value)
assertArray ts = \case
  Array vec -> pure vec
  v -> throwError $ InvalidArrayOperation ts v

assertUnsnoc
  :: MonadError PatchError m
  => [Token]
  -> Vector Value
  -> m (Vector Value, Value)
assertUnsnoc ts vec =
  case V.unsnoc vec of
    Nothing -> throwError $ EmptyArray ts
    Just tp -> pure tp
