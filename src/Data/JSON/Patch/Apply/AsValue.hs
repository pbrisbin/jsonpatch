-- |
--
-- Module      : Data.JSON.Patch.Apply.AsValue
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.Patch.Apply.AsValue
  ( PatchError (..)
  , patchAsValue
  ) where

import Data.JSON.Patch.Prelude

import Data.Aeson (FromJSON, Result (..), ToJSON (..), Value (..), fromJSON)
import Data.Aeson.Optics (AsValue (..))
import Data.JSON.Patch.Apply
import Optics.Core

-- | A polymorphic version of 'patchValue'
--
-- The @patch@ input uses 'AsValue' from @aeson-optics@, meaning you can supply
-- a variety of types such as 'ByteString' or 'Value' and it will be parsed into
-- @['Patches']@ (capturing failure as a 'PatchError').
--
-- The @v@ input can be any domain type with JSON instances. We don't use
-- 'AsValue' here as well, even though it provides the same functionality,
-- because it's unlikely your types will have this instance.
--
-- @
-- data Person = Person
--   { name :: Text
--   , age :: Int
--   }
--   deriving stock Generic
--   deriving anyclass (FromJSON, ToJSON)
--
-- patchPersonR :: PersonId -> Handler Person
-- patchPersonR id = do
--   person <- runDB $ get id           -- Person "pat" 19
--   bytes <- getRequestBody            -- "[{op:replace, path:/age, value:21}]"
--
--   case patchAsValue bytes person of
--     Left err -> sendResponse 400 $ displayException err
--     Right updated -> do
--       runDB $ update id updated
--       sendResponse 200 updated       -- Person "pat" 21
-- @
--
-- If the patch creates a value that can't parse back to your domain type, that
-- will also be normalized to 'PatchError'.
patchAsValue
  :: (AsValue patch, FromJSON v, ToJSON v) => patch -> v -> Either PatchError v
patchAsValue p target = do
  pVal <- note (ParseError Null "not JSON") $ p ^? _Value
  patches <- fromJSONEither pVal
  result <- patchValue patches $ toJSON target
  fromJSONEither result

fromJSONEither :: FromJSON a => Value -> Either PatchError a
fromJSONEither v = case fromJSON v of
  Error e -> Left $ ParseError v e
  Success a -> Right a
