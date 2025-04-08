module Data.JSON.Patch.Apply
  ( applyPatches
  ) where

import Prelude

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
  Add op -> add op.value op.path
  Remove op -> remove op.path
  Replace op -> replace op.value op.path
  Move op -> flip replace op.path =<< get op.from
  Copy op -> flip add op.path =<< get op.from
  Test {} -> error "TODO"

get :: (MonadError String m, MonadState Value m) => Pointer -> m Value
get p = case p of
  PointerEmpty -> gets id
  PointerPath ts t -> do
    gets (preview $ tokensL ts % tokenL t) >>= \case
      Nothing -> pointerError "the specified value does not exist"
      Just v -> pure v
  PointerPathEnd ts ->
    gets (preview $ tokensL ts % _Array % to lastMaybeV) >>= \case
      Nothing -> pointerError "the specified value does not exist"
      Just Nothing -> pointerError "the specified array is empty"
      Just (Just v) -> pure v
 where
  pointerError :: MonadError String m => String -> m a
  pointerError msg = throwError $ pointerToString p <> ": " <> msg

lastMaybeV :: Vector a -> Maybe a
lastMaybeV v = snd <$> V.unsnoc v

add :: MonadState Value m => Value -> Pointer -> m ()
add v = \case
  PointerEmpty -> put v
  PointerPath ts t -> case t of
    K k -> modify $ tokensL ts % _Object % at k ?~ v
    N _ -> error "TODO"
  PointerPathEnd ts -> modify $ tokensL ts % _Array %~ (<> pure v)

remove :: MonadState Value m => Pointer -> m ()
remove = \case
  PointerEmpty -> error "TODO"
  PointerPath ts t -> case t of
    K k -> modify $ tokensL ts % _Object % at k .~ Nothing
    N _ -> error "TODO"
  PointerPathEnd {} -> error "TODO"

replace :: MonadState Value m => Value -> Pointer -> m ()
replace v p = remove p >> add v p
