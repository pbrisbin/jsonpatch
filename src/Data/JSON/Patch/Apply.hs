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
  ) where

import Prelude

import Control.Monad (unless, void, when)
import Control.Monad.Except (MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, execStateT, gets, modify, put)
import Data.Aeson
import Data.Aeson.Optics
import Data.Foldable (traverse_)
import Data.JSON.Patch.Type
import Data.JSON.Pointer
import Data.Vector (Vector)
import Data.Vector qualified as V
import Optics

applyPatches :: [Patch] -> Value -> Either String Value
applyPatches ps = runExcept . execStateT (traverse_ applyPatch ps)

applyPatch :: (MonadError String m, MonadState Value m) => Patch -> m ()
applyPatch = \case
  Add op -> do
    assertArrayBounds' op.path
    add op.value op.path
  Remove op -> do
    assertPointer op.path
    assertArrayBounds op.path
    remove op.path
  Replace op -> remove op.path >> add op.value op.path
  Move op -> flip add op.path =<< get op.from
  Copy op -> flip add op.path =<< get op.from
  Test op -> do
    v <- get op.path
    unless (v == op.value)
      $ pointerError op.path
      $ "test failed: "
        <> show v
        <> " != "
        <> show op.value

assertPointer :: (MonadError String m, MonadState Value m) => Pointer -> m ()
assertPointer p = void $ get p

assertArrayBounds
  :: (MonadError String m, MonadState Value m)
  => Pointer
  -> m ()
assertArrayBounds p = case p of
  PointerEmpty -> pure ()
  PointerPath _ t -> do
    v <- get p -- we've already asserted this won't fail
    case (v, t) of
      (Array vec, N n) ->
        when (n >= V.length vec)
          $ pointerError p
          $ "index " <> show n <> " is out of bounds in " <> show vec
      _ -> pure ()
  PointerPathEnd _ -> pure ()

assertArrayBounds'
  :: (MonadError String m, MonadState Value m)
  => Pointer
  -> m ()
assertArrayBounds' p = case p of
  PointerEmpty -> pure ()
  PointerPath ts t -> do
    gets (preview $ tokensL ts) >>= \case
      Nothing -> pure ()
      Just v ->
        case (v, t) of
          (Array vec, N n) ->
            when (n > V.length vec)
              $ pointerError p
              $ "index " <> show n <> " is out of bounds in " <> show vec
          _ -> pure ()
  PointerPathEnd _ -> pure ()

get :: (MonadError String m, MonadState Value m) => Pointer -> m Value
get = \case
  PointerEmpty -> gets id
  PointerPath ts t -> do
    gets (preview $ tokensL ts % atTokenL t) >>= \case
      Nothing -> tokensError ts "the specified value does not exist"
      Just Nothing -> tokensError (ts <> [t]) "the specified value does not exist"
      Just (Just v) -> pure v
  PointerPathEnd ts -> withVectorUnsnoc ts $ pure . snd

add :: (MonadError String m, MonadState Value m) => Value -> Pointer -> m ()
add v = \case
  PointerEmpty -> put v
  PointerPath ts t -> modify $ tokensL ts % atTokenL t ?~ v
  PointerPathEnd ts -> withVector ts $ \_ ->
    modify $ tokensL ts % _Array %~ (<> pure v)

remove :: (MonadError String m, MonadState Value m) => Pointer -> m ()
remove = \case
  PointerEmpty -> put Null -- unspecified behavior
  PointerPath ts t -> modify $ tokensL ts % atTokenL t .~ Nothing
  PointerPathEnd ts -> withVectorUnsnoc ts $ \(vs, _) ->
    modify $ tokensL ts % _Array .~ vs

withVector
  :: (MonadError String m, MonadState Value m)
  => [Token]
  -> (Vector Value -> m a)
  -> m a
withVector ts f =
  gets (preview $ tokensL ts % _Array) >>= \case
    Nothing -> tokensError ts "the specified value doesn't exist or is not an array"
    Just vs -> f vs

withVectorUnsnoc
  :: (MonadError String m, MonadState Value m)
  => [Token]
  -> ((Vector Value, Value) -> m a)
  -> m a
withVectorUnsnoc ts f =
  gets (preview $ tokensL ts % _Array) >>= \case
    Nothing -> tokensError ts "the specified value doesn't exist or is not an array"
    Just vs -> case V.unsnoc vs of
      Nothing -> tokensError ts "the specified array is empty"
      Just tp -> f tp

pointerError :: MonadError String m => Pointer -> String -> m a
pointerError p msg = throwError $ pointerToString p <> ": " <> msg

tokensError :: MonadError String m => [Token] -> String -> m a
tokensError ts msg = throwError $ tokensToString ts <> ": " <> msg
