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

import Prelude

import Data.Aeson (Key, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Optics (key, nth)
import Data.Aeson.Optics.Ext
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Optics.Core
import Text.Read (readEither)

data Token = N Int | E | K Key
  deriving stock (Eq, Show)

-- | Access a key or array index like 'ix', used for indexing
tokenL :: Token -> AffineTraversal' Value Value
tokenL t = case t of
  N n -> nth n
  E -> atEnd % _Just
  K k -> key k

-- | Access a key or array index like 'at', used for adding or removing
atTokenL :: Token -> AffineTraversal' Value (Maybe Value)
atTokenL = \case
  N n -> atNth n
  E -> atEnd
  K k -> atKey k

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
  | T.isPrefixOf "0" t = Left "leading zeros"
  | otherwise =
      first (\msg -> "could not read digits " <> unpack t <> ": " <> msg)
        $ readEither
        $ unpack t

tokenToText :: Token -> Text
tokenToText = \case
  K k -> T.replace "/" "~1" $ T.replace "~" "~0" $ Key.toText k
  N n -> pack $ show n
  E -> "-"
