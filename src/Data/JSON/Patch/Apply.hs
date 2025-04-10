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

import Data.JSON.Patch.Prelude

import Data.Aeson (Value (..))
import Data.JSON.Patch.Error
import Data.JSON.Patch.Type
import Data.JSON.Pointer
import Data.JSON.Pointer.Token
import Data.Vector qualified as V
import Optics.Core

applyPatches :: [Patch] -> Value -> Either PatchError Value
applyPatches ps v = foldM applyPatch v ps

applyPatch :: Value -> Patch -> Either PatchError Value
applyPatch val = \case
  Add op -> add op.value op.path val
  Remove op -> remove op.path val
  Replace op -> remove op.path val >>= add op.value op.path
  Move op -> do
    v <- get op.from val
    remove op.from val >>= add v op.path
  Copy op -> do
    v <- get op.from val
    add v op.path val
  Test op -> do
    v <- get op.path val
    if v /= op.value
      then Left $ TestFailed op.path v op.value
      else Right val

get :: Pointer -> Value -> Either PatchError Value
get p val =
  maybe (Left $ PointerNotFound p Nothing) Right
    $ preview (pointerL p) val

add :: Value -> Pointer -> Value -> Either PatchError Value
add v p val = case splitPointer p of
  Nothing -> Right v
  Just (parent, t) -> do
    validateAdd parent t val
    Right $ val & atPointerL p ?~ v

remove :: Pointer -> Value -> Either PatchError Value
remove p val = do
  void $ get p val
  Right $ val & atPointerL p .~ Nothing

validateAdd :: Pointer -> Token -> Value -> Either PatchError ()
validateAdd parent t val = do
  target <- get parent val

  case (t, target) of
    (_, Object _) -> Right () -- everything works on objects
    (K _, v) -> Left $ InvalidObjectOperation parent v
    (N n, Array vec) -> do
      when (n < 0) $ Left $ IndexOutOfBounds parent n vec
      when (n > V.length vec) $ Left $ IndexOutOfBounds parent n vec
    (N _, v) -> Left $ InvalidArrayOperation parent v
    _ -> Right ()
