module Data.Aeson.Optics.Ext
  ( atKey
  , atNth
  , atNth'
  ) where

import Prelude

import Data.Aeson (Key, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Optics
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Ext qualified as V
import Optics

-- | Like 'key', but uses 'at' instead of 'ix'. This is handy when adding and
-- removing object keys:
--
-- >>> "{\"a\": 100, \"b\": 200}" & atKey "a" .~ Nothing
-- "{\"b\":200}"
--
-- >>> "{\"a\": 100, \"b\": 200}" & atKey "c" ?~ String "300"
-- "{\"a\":100,\"b\":200,\"c\":\"300\"}"
atKey :: Key -> AffineTraversal' Value (Maybe Value)
atKey k = _Object % at k

-- | Like 'atKey' but for 'Array'
--
-- This isn't as simple as using 'at' because the semantics aren't clear.
--
-- 1. Any index out of bounds leaves the object as-is
-- 2. Setting works as you probably expect
-- 3. Removal will __shift all later elements left__
atNth :: Int -> AffineTraversal' Value (Maybe Value)
atNth n = atraversal (nMatcher n) $ nUpdater V.setAt n

-- | A version of 'atNth' that treats setting as insertion
--
-- 1. Any index out of bounds leaves the object as-is
-- 2. Removal works like 'atNth'
-- 3. Setting (inserting) will __shift all later elements right__
atNth' :: Int -> AffineTraversal' Value (Maybe Value)
atNth' n = atraversal (nMatcher n) $ nUpdater V.insertAt n

nMatcher :: Int -> Value -> Either Value (Maybe Value)
nMatcher n = \case
  Object km -> Right $ KeyMap.lookup (Key.fromString $ show n) km
  Array vec -> Right $ vec V.!? n
  v -> Left v

nUpdater
  :: (Int -> Value -> Vector Value -> Vector Value)
  -- ^ How to update the vector, insertAt or setAt?
  -> Int
  -> Value
  -> Maybe Value
  -> Value
nUpdater f n nv = \case
  Nothing -> case nv of
    Object km -> Object $ KeyMap.delete (Key.fromString $ show n) km
    Array vec -> Array $ V.deleteAt n vec
    v -> v
  Just x -> case nv of
    Object km -> Object $ KeyMap.insert (Key.fromString $ show n) x km
    Array vec -> Array $ f n x vec
    v -> v
