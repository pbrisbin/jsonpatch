-- |
--
-- Module      : Data.JSON.Patch.Apply
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.Patch.Apply
  ( PatchError (..)
  , patchValue
  ) where

import Data.JSON.Patch.Prelude

import Data.Aeson (Value (..))
import Data.JSON.Patch.Error
import Data.JSON.Patch.Type
import Data.JSON.Pointer
import Data.JSON.Pointer.Token
import Data.Vector qualified as V
import Optics.Core

-- | Apply the given 'Patch'es to the given 'Value'
patchValue :: [Patch] -> Value -> Either PatchError Value
patchValue patches target = foldM go target patches
 where
  go :: Value -> Patch -> Either PatchError Value
  go val = \case
    Add op -> add op.value op.path val
    Remove op -> remove op.path val
    Replace op -> remove op.path val >>= add op.value op.path
    Move op -> get op.from val >>= \v -> remove op.from val >>= add v op.path
    Copy op -> get op.from val >>= \v -> add v op.path val
    Test op -> get op.path val >>= \v -> test v op.value op.path val

get :: Pointer -> Value -> Either PatchError Value
get p val = note (PointerNotFound p Nothing) $ val ^? pointerL p

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

test :: Value -> Value -> Pointer -> Value -> Either PatchError Value
test v expected p val =
  note (TestFailed p v expected) $ val <$ guard (v == expected)

validateAdd :: Pointer -> Token -> Value -> Either PatchError ()
validateAdd parent t val = do
  target <- get parent val

  case (t, target) of
    (_, Object _) -> Right () -- everything works on objects
    (K _, nonObject) -> Left $ InvalidObjectOperation parent nonObject
    (N n, Array vec) -> do
      when (n < 0) $ Left $ IndexOutOfBounds parent n vec
      when (n > V.length vec) $ Left $ IndexOutOfBounds parent n vec
    (N _, nonArray) -> Left $ InvalidArrayOperation parent nonArray
    _ -> Right ()
