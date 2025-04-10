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
  , tokenP
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

tokenFromText :: Text -> Either String Token
tokenFromText = parseOnly tokenP

tokenP :: Parser Token
tokenP =
  choice
    [ nP <?> "numeric index without leading zeros"
    , eP <?> "end of array token"
    , kP <?> "escaped object key"
    ]

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

tokenToText :: Token -> Text
tokenToText = \case
  K k -> T.replace "/" "~1" $ T.replace "~" "~0" $ Key.toText k
  N n -> pack $ show n
  E -> "-"
