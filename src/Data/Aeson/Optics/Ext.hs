-- |
--
-- Module      : Data.Aeson.Optics.Ext
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.Aeson.Optics.Ext
  ( atKey
  , atNth
  , atEnd
  ) where

import Prelude

import Data.Aeson (Key, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Optics
import Data.Vector qualified as V
import Data.Vector.Ext qualified as V
import Optics.Core

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

-- | Like 'atKey', but for 'Array's
--
-- Adding shifts all later elements right:
--
-- >>> ['a', 'b'] & atNth 1 ?~ 'x'
-- ['a', 'x', 'b']
--
-- Removing shifts all later elements left:
--
-- >>> ['a', 'b', 'c'] & atNth 1 .~ Nothing
-- ['a', 'c']
--
-- __NOTE__: this function will also index objects, in which case this behaves
-- exactly like 'atKey'. This is necessary for our use-case and probably means
-- we could never upstream this.
--
-- >>> {"0": 'a', "1": 'b'} & atNth 1 ?~ 'x'
-- {"0": 'a', "1": 'x', "2": 'b'}
atNth :: Int -> AffineTraversal' Value (Maybe Value)
atNth n = atraversal matcher updater
 where
  matcher :: Value -> Either Value (Maybe Value)
  matcher = \case
    Object km -> Right $ KeyMap.lookup (Key.fromString $ show n) km
    Array vec -> Right $ vec V.!? n
    v -> Left v

  updater :: Value -> Maybe Value -> Value
  updater nv = \case
    Nothing -> case nv of
      Object km -> Object $ KeyMap.delete (Key.fromString $ show n) km
      Array vec -> Array $ V.deleteAt n vec
      v -> v
    Just x -> case nv of
      Object km -> Object $ KeyMap.insert (Key.fromString $ show n) x km
      Array vec -> Array $ V.insertAt n x vec
      v -> v

-- | List 'atNth' for the index /after/ the last value of an 'Array'
--
-- This only useful to normalize adds as a lens:
--
-- >>> ['a', 'b', 'c'] & atEnd ?~ 'x'
-- ['a', 'b', 'c', 'x']
--
-- Attempting to access this index on an array will always given 'Nothing'
--
-- >>> ['a', 'b', 'c'] ^? atEnd
-- Nothing
--
-- Most other operations are going to be a no-op.
atEnd :: AffineTraversal' Value (Maybe Value)
atEnd = atraversal matcher updater
 where
  matcher :: Value -> Either Value (Maybe Value)
  matcher = \case
    Array {} -> Right Nothing -- index after end doesn't exist
    v -> Left v

  updater :: Value -> Maybe Value -> Value
  updater nv = \case
    Nothing -> nv
    Just x -> case nv of
      Array vec -> Array $ vec <> pure x
      v -> v
