-- |
--
-- Module      : Data.JSON.Pointer.Token
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.Pointer.Token
  ( Token (..)
  , tokenFromText
  , tokenToText
  , tokenL
  , atTokenL
  ) where

import Data.JSON.Patch.Prelude

import Data.Aeson (Key, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Optics (key, nth)
import Data.Aeson.Optics.Ext
import Data.Char (isDigit)
import Data.Text qualified as T
import Optics.Core
import Text.Read (readMaybe)

data Token = K Key | N Int | E
  deriving stock (Eq, Show)

-- | Access a key or array index like 'ix', used for indexing
tokenL :: Token -> AffineTraversal' Value Value
tokenL t = case t of
  K k -> key k
  N n -> nth n
  E -> atEnd % _Just

-- | Access a key or array index like 'at', used for adding or removing
atTokenL :: Token -> AffineTraversal' Value (Maybe Value)
atTokenL = \case
  K k -> atKey k
  N n -> atNth n
  E -> atEnd

tokenFromText :: Text -> Either String Token
tokenFromText = \case
  "" -> Right $ K ""
  "-" -> Right E
  t | T.all isDigit t -> N <$> readDigits t
  -- The spec doesn't allow this (negative indexes), but the tests use it to
  -- trigger the lower bounds error example.
  t | Just ('-', n) <- T.uncons t, T.all isDigit n -> N . negate <$> readDigits n
  t -> Right $ K $ Key.fromText $ T.replace "~0" "~" $ T.replace "~1" "/" t

readDigits :: Text -> Either String Int
readDigits t
  | t == "0" = Right 0
  | T.isPrefixOf "0" t = Left "tokens cannot have leading zeros"
  | otherwise =
      note ("could not read " <> show t <> " as integer")
        $ readMaybe
        $ unpack t

tokenToText :: Token -> Text
tokenToText = \case
  K k -> T.replace "/" "~1" $ T.replace "~" "~0" $ Key.toText k
  N n -> pack $ show n
  E -> "-"
