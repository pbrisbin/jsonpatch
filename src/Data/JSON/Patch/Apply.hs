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

import Control.Monad (unless, void)
import Control.Monad.Except (MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, execStateT, gets, modify, put)
import Data.Aeson
import Data.Aeson.Optics
import Data.Foldable (for_, traverse_)
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
  PointerPath ts t -> do
    gets (preview $ tokensL ts % tokenL t) >>= \case
      Nothing -> throwError $ PointerNotFound ts Nothing
      Just v -> pure v
  PointerPathEnd ts -> snd <$> assertArrayUnsnoc ts

add :: (MonadError PatchError m, MonadState Value m) => Value -> Pointer -> m ()
add v = \case
  PointerEmpty -> put v
  PointerPath ts t -> do
    validateAdd ts t
    modify $ tokensL ts % atTokenL t ?~ v
  PointerPathEnd ts -> do
    void $ assertArray ts
    modify $ tokensL ts % _Array %~ (<> pure v)

validateAdd
  :: (MonadError PatchError m, MonadState Value m)
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

remove :: (MonadError PatchError m, MonadState Value m) => Pointer -> m ()
remove = \case
  PointerEmpty -> put Null -- unspecified behavior
  PointerPath ts t -> do
    validateRemove ts t
    modify $ tokensL ts % atTokenL t .~ Nothing
  PointerPathEnd ts -> do
    (vs, _) <- assertArrayUnsnoc ts
    modify $ tokensL ts % _Array .~ vs

validateRemove
  :: (MonadError PatchError m, MonadState Value m)
  => [Token]
  -> Token
  -> m ()
validateRemove ts t = do
  -- removing something non-existent
  void $ assertExists $ ts <> [t]
  -- removing outside of bounds
  withN t $ assertBounds (>=) $ ts <> [t]

assertExists
  :: (MonadError PatchError m, MonadState Value m) => [Token] -> m Value
assertExists ts = do
  mv <- gets $ preview $ tokensL ts
  case mv of
    Nothing -> throwError $ PointerNotFound ts Nothing
    Just v -> pure v

assertObject :: MonadError PatchError m => [Token] -> Value -> m ()
assertObject ts = \case
  Object {} -> pure ()
  v -> throwError $ InvalidObjectOperation ts v

assertBounds
  :: (MonadError PatchError m, MonadState Value m)
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
    Array vec | n `p` V.length vec -> throwError $ IndexOutOfBounds ts n vec
    _ -> pure ()

assertArray
  :: (MonadError PatchError m, MonadState Value m)
  => [Token]
  -> m (Vector Value)
assertArray ts =
  gets (preview $ tokensL ts % _Array) >>= \case
    Nothing -> throwError $ PointerNotFound ts $ Just "Array"
    Just vs -> pure vs

assertArrayUnsnoc
  :: (MonadError PatchError m, MonadState Value m)
  => [Token]
  -> m (Vector Value, Value)
assertArrayUnsnoc ts =
  gets (preview $ tokensL ts % _Array) >>= \case
    Nothing -> throwError $ PointerNotFound ts $ Just "Array"
    Just vs -> case V.unsnoc vs of
      Nothing -> throwError $ EmptyArray ts
      Just tp -> pure tp

withK :: Applicative f => Token -> (Key -> f ()) -> f ()
withK t f = case t of
  K k -> f k
  _ -> pure ()

withN :: Applicative f => Token -> (Int -> f ()) -> f ()
withN t f = case t of
  N n -> f n
  _ -> pure ()
