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
import Data.Foldable (for_, traverse_)
import Data.JSON.Patch.Type
import Data.JSON.Pointer
import Data.Vector (Vector)
import Data.Vector qualified as V
import Optics

applyPatches :: [Patch] -> Value -> Either String Value
applyPatches ps = runExcept . execStateT (traverse_ applyPatch ps)

applyPatch :: (MonadError String m, MonadState Value m) => Patch -> m ()
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
    unless (v == op.value)
      $ pointerError op.path
      $ "test failed: "
        <> show v
        <> " != "
        <> show op.value

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
  PointerPath ts t -> do
    case t of
      N n -> assertBounds (>) ts n
      _ -> pure ()

    modify $ tokensL ts % atTokenL t ?~ v
  PointerPathEnd ts -> withVector ts $ \_ ->
    modify $ tokensL ts % _Array %~ (<> pure v)

remove :: (MonadError String m, MonadState Value m) => Pointer -> m ()
remove p = do
  void $ get p -- assert removing a pointer must exist
  case p of
    PointerEmpty -> put Null -- unspecified behavior
    PointerPath ts t -> do
      case t of
        N n -> assertBounds (>=) (ts <> [t]) n
        _ -> pure ()

      modify $ tokensL ts % atTokenL t .~ Nothing
    PointerPathEnd ts -> withVectorUnsnoc ts $ \(vs, _) ->
      modify $ tokensL ts % _Array .~ vs

assertBounds
  :: (MonadError String m, MonadState Value m)
  => (Int -> Int -> Bool)
  -- ^ Validation predicate, '(>)' for adds, '(>=)' for removes
  -> [Token]
  -- ^ Tokens to object to validate (if an 'Array')
  -> Int
  -- ^ First argument to validation (e.g. index). Passed separately so it can be
  -- used in error messages
  -> m ()
assertBounds p ts n = do
  mv <- gets $ preview $ tokensL ts
  for_ mv $ \case
    Array vec ->
      when (n `p` V.length vec)
        $ tokensError ts
        $ "index "
          <> show n
          <> " is out of bounds for "
          <> show vec
    _ -> pure ()

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
