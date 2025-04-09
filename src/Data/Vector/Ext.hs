module Data.Vector.Ext
  ( deleteAt
  , insertAt
  , setAt
  ) where

import Prelude

import Data.Vector (Vector)
import Data.Vector qualified as V

-- | Delete from a vector and shift all later elements left
deleteAt :: Int -> Vector a -> Vector a
deleteAt n vec = generateFrom $ V.imapMaybe shift vec
 where
  shift idx a
    | idx < n = Just (idx, a)
    | idx == n = Nothing
    | otherwise = Just (idx - 1, a)

-- | Insert into a vector and shift all later elements right
insertAt :: Int -> a -> Vector a -> Vector a
insertAt n v vec = generateFrom $ V.imap shift vec <> pure (n, v)
 where
  shift idx a
    | idx >= n = (idx + 1, a)
    | otherwise = (idx, a)

-- | Update an index in a vector
setAt :: Int -> a -> Vector a -> Vector a
setAt n v vec = generateFrom $ V.imap replace vec
 where
  replace idx a
    | idx == n = (idx, v)
    | otherwise = (idx, a)

generateFrom :: Vector (Int, a) -> Vector a
generateFrom indexed = V.generate (V.length indexed) $ snd . (V.!) indexed
