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
import Data.Vector qualified as V
import Data.Vector.Ext qualified as V
import Optics

atKey :: Key -> AffineTraversal' Value (Maybe Value)
atKey k = _Object % at k

atNth :: Int -> AffineTraversal' Value (Maybe Value)
atNth n = atraversal (nMatcher n) (nUpdater n)

atNth' :: Int -> AffineTraversal' Value (Maybe Value)
atNth' n = atraversal (nMatcher n) (nInserter n)

nMatcher :: Int -> Value -> Either Value (Maybe Value)
nMatcher n = \case
  Object km -> Right $ KeyMap.lookup (Key.fromString $ show n) km
  Array vec -> Right $ vec V.!? n
  v -> Left v

nUpdater :: Int -> Value -> Maybe Value -> Value
nUpdater n nv = \case
  Just x | Array vec <- nv -> Array $ V.setAt n x vec
  v -> nInserter n nv v -- all other cases are the same

nInserter :: Int -> Value -> Maybe Value -> Value
nInserter n nv = \case
  Nothing -> case nv of
    Object km -> Object $ KeyMap.delete (Key.fromString $ show n) km
    Array vec -> Array $ V.deleteAt n vec
    v -> v
  Just x -> case nv of
    Object km -> Object $ KeyMap.insert (Key.fromString $ show n) x km
    Array vec -> Array $ V.insertAt n x vec
    v -> v
