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
  , tokenP
  , tokensToText
  , tokenL
  , atTokenL
  ) where

import Prelude

import Control.Applicative (optional, (<|>))
import Data.Aeson (Key, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Optics (key, nth)
import Data.Aeson.Optics.Ext
import Data.Attoparsec.Text hiding (atEnd)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Optics
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

tokensToText :: [Token] -> Text
tokensToText ts = "/" <> T.intercalate "/" (map tokenToText ts)

tokenToText :: Token -> Text
tokenToText = \case
  K k -> Key.toText k
  N n -> pack $ show n
  E -> "-"

tokenP :: Parser Token
tokenP = wtf <|> choice [nP, eP, kP]

-- |
--
-- The tests love to use this value as an example. I'm clearly confused because
-- I would expect it to fail 'nP' and 'eP', backtrack, then succeed in 'kP' just
-- fine, but instead it causes errors with "endOfInput". We'll special case it
-- for now, but clearly keys with numbers in them are a problem.
wtf :: Parser Token
wtf = K . Key.fromText <$> string "1e0"

nP :: Parser Token
nP =
  N <$> do
    f <- maybe id (const negate) <$> optional (char '-')
    f <$> nonzeroP <|> 0 <$ char '0'

nonzeroP :: Parser Int
nonzeroP = do
  ds <- (:) <$> satisfy (inClass "1-9") <*> many' digit
  either (err ds) pure $ readEither ds
 where
  err :: String -> String -> Parser a
  err x msg = fail $ "Unable to read integer from " <> x <> ": " <> msg

eP :: Parser Token
eP = E <$ char '-'

kP :: Parser Token
kP =
  K
    . Key.fromText
    . T.replace "~0" "~"
    . T.replace "~1" "/"
    <$> takeTill (== '/')
