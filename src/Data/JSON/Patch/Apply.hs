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
import Data.JSON.Pointer.Token
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
    gets (preview $ tokensL ts % tokenL t) >>= \case
      Nothing -> tokensError ts "the specified value does not exist"
      Just v -> pure v
  PointerPathEnd ts -> snd <$> assertArrayUnsnoc ts

add :: (MonadError String m, MonadState Value m) => Value -> Pointer -> m ()
add v = \case
  PointerEmpty -> put v
  PointerPath ts t -> do
    validateAdd ts t
    modify $ tokensL ts % atTokenL t ?~ v
  PointerPathEnd ts -> do
    void $ assertArray ts
    modify $ tokensL ts % _Array %~ (<> pure v)

validateAdd
  :: (MonadError String m, MonadState Value m)
  => [Token]
  -> Token
  -> m ()
validateAdd ts t = do
  -- adding to something non-existent is an error
  target <- assertExists ts
  -- object operation on non-object
  withK t $ \_ -> assertObject ts target
  -- adding outside of bounds is an error
  withN t $ assertBounds (>) ts

remove :: (MonadError String m, MonadState Value m) => Pointer -> m ()
remove = \case
  PointerEmpty -> put Null -- unspecified behavior
  PointerPath ts t -> do
    validateRemove ts t
    modify $ tokensL ts % atTokenL t .~ Nothing
  PointerPathEnd ts -> do
    (vs, _) <- assertArrayUnsnoc ts
    modify $ tokensL ts % _Array .~ vs

validateRemove
  :: (MonadError String m, MonadState Value m)
  => [Token]
  -> Token
  -> m ()
validateRemove ts t = do
  -- removing something non-existent
  void $ assertExists $ ts <> [t]
  -- removing outside of bounds
  withN t $ assertBounds (>=) $ ts <> [t]

assertExists :: (MonadError String m, MonadState Value m) => [Token] -> m Value
assertExists ts = do
  mv <- gets $ preview $ tokensL ts
  case mv of
    Nothing -> tokensError ts "the specified value doesn't exist"
    Just v -> pure v

assertObject :: MonadError String m => [Token] -> Value -> m ()
assertObject ts = \case
  Object {} -> pure ()
  _ -> tokensError ts "Object operation on non-object"

assertBounds
  :: (MonadError String m, MonadState Value m)
  => (Int -> Int -> Bool)
  -- ^ Validation predicate, '(>)' for adds, '(>=)' for removes
  -> [Token]
  -- ^ Tokens to object to validate (if an 'Array')
  -> Int
  -- ^ First argument to validation (e.g. index)
  --
  -- Passed separately so it can be used in error messages
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

assertArray
  :: (MonadError String m, MonadState Value m)
  => [Token]
  -> m (Vector Value)
assertArray ts =
  gets (preview $ tokensL ts % _Array) >>= \case
    Nothing -> tokensError ts "the specified value doesn't exist or is not an array"
    Just vs -> pure vs

assertArrayUnsnoc
  :: (MonadError String m, MonadState Value m)
  => [Token]
  -> m (Vector Value, Value)
assertArrayUnsnoc ts =
  gets (preview $ tokensL ts % _Array) >>= \case
    Nothing -> tokensError ts "the specified value doesn't exist or is not an array"
    Just vs -> case V.unsnoc vs of
      Nothing -> tokensError ts "the specified array is empty"
      Just tp -> pure tp

withK :: Applicative f => Token -> (Key -> f ()) -> f ()
withK t f = case t of
  K k -> f k
  _ -> pure ()

withN :: Applicative f => Token -> (Int -> f ()) -> f ()
withN t f = case t of
  N n -> f n
  _ -> pure ()

pointerError :: MonadError String m => Pointer -> String -> m a
pointerError p msg = throwError $ pointerToString p <> ": " <> msg

tokensError :: MonadError String m => [Token] -> String -> m a
tokensError ts msg = throwError $ tokensToString ts <> ": " <> msg
