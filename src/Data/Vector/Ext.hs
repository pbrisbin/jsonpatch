-- |
--
-- Module      : Data.Vector.Ext
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.Vector.Ext
  ( deleteAt
  , insertAt
  ) where

import Prelude

import Data.Vector (Vector)
import Data.Vector qualified as V

-- | Delete from a vector and shift all later elements left
deleteAt :: Int -> Vector a -> Vector a
deleteAt n vec
  | n < 0 = vec
  | n >= V.length vec = vec
  | otherwise = generateFrom $ V.imapMaybe shift vec
 where
  shift idx a
    | idx < n = Just (idx, a)
    | idx == n = Nothing
    | otherwise = Just (idx - 1, a)

-- | Insert into a vector and shift all later elements right
insertAt :: Int -> a -> Vector a -> Vector a
insertAt n v vec
  | n < 0 = vec
  | n > V.length vec = vec
  | otherwise = generateFrom $ V.imap shift vec <> pure (n, v)
 where
  shift idx a
    | idx >= n = (idx + 1, a)
    | otherwise = (idx, a)

generateFrom :: Vector (Int, a) -> Vector a
generateFrom indexed = V.generate (length indexed) $ \idx ->
  maybe (badIndex idx) snd $ V.find ((== idx) . fst) indexed
 where
  badIndex idx =
    error
      $ "Index "
        <> show idx
        <> " is not present in vector of indexed values: "
        <> show indexes

  indexes :: [Int]
  indexes = map fst $ V.toList indexed
