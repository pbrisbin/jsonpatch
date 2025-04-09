-- <https://datatracker.ietf.org/doc/html/rfc6901/>
module Data.JSON.Pointer
  ( Pointer (..)
  , pointerFromText
  , pointerToText
  , pointerToString
  , Token (..)
  , tokensToText
  , tokensToString
  , tokensL
  , tokenL
  , atTokenL
  ) where

import Prelude

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Key, Value (..), withText)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Optics
import Data.Aeson.Optics.Ext
import Data.Attoparsec.Text
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Optics
import Text.Read (readEither)

data Pointer
  = -- | @""@ means whole-document
    PointerEmpty
  | -- | @"/[.../]x"@ means path to a key or index @x@
    --
    -- NB. @"/"@ naturally becomes @'PointerPath' [] ('K' "")@
    PointerPath [Token] Token
  | -- | @"/[.../]-"@ means path to last element of an array
    PointerPathEnd [Token]
  deriving stock (Eq, Show)

instance FromJSON Pointer where
  parseJSON = withText "Pointer" $ either fail pure . pointerFromText

pointerFromText :: Text -> Either String Pointer
pointerFromText = \case
  "" -> Right PointerEmpty
  "/" -> Right $ PointerPath [] $ K ""
  "/-" -> Right $ PointerPathEnd []
  p -> case T.stripSuffix "/-" p of
    Nothing -> parseOnly pointerPathP p
    Just p' -> parseOnly pointerPathEndP p'

pointerPathP :: Parser Pointer
pointerPathP = do
  ts <- maybe (fail "") pure . nonEmpty =<< pointerP
  pure $ PointerPath (NE.init ts) $ NE.last ts

pointerPathEndP :: Parser Pointer
pointerPathEndP = PointerPathEnd <$> pointerP

pointerP :: Parser [Token]
pointerP = char '/' *> tokenP `sepBy1` char '/' <* endOfInput

tokenP :: Parser Token
tokenP = wtf <|> N <$> indexP <|> K <$> keyP

-- |
--
-- The tests love to use this value as an example. I'm clearly confused because
-- I would expect it to fail 'indexP', backtrack, then succeed in 'keyP', but it
-- causes errors with "endOfInput", so we'll special case it for now.
wtf :: Parser Token
wtf = K . Key.fromText <$> string "1e0"

keyP :: Parser Key
keyP =
  Key.fromText
    . T.replace "~0" "~"
    . T.replace "~1" "/"
    <$> takeTill (== '/')

indexP :: Parser Int
indexP = nonzeroP <|> 0 <$ char '0'

nonzeroP :: Parser Int
nonzeroP = do
  ds <- (:) <$> satisfy (inClass "1-9") <*> many' digit
  either (err ds) pure $ readEither ds
 where
  err :: String -> String -> Parser a
  err x msg = fail $ "Unable to read integer from " <> x <> ": " <> msg

pointerToText :: Pointer -> Text
pointerToText = \case
  PointerEmpty -> ""
  PointerPath ts t -> tokensToText $ ts <> [t]
  PointerPathEnd ts -> tokensToText ts <> "/-"

pointerToString :: Pointer -> String
pointerToString = unpack . pointerToText

data Token = K Key | N Int
  deriving stock (Eq, Show)

tokensToString :: [Token] -> String
tokensToString = unpack . tokensToText

tokensToText :: [Token] -> Text
tokensToText ts = "/" <> T.intercalate "/" (map tokenToText ts <> ["-"])

tokenToText :: Token -> Text
tokenToText = \case
  K k -> Key.toText k
  N n -> pack $ show n

tokensL :: [Token] -> AffineTraversal' Value Value
tokensL = foldr ((%) . tokenL) $ castOptic simple

-- | Access a key or array index like 'ix'
tokenL :: Token -> AffineTraversal' Value Value
tokenL t = case t of
  K k -> key k
  N n -> nth n

-- | Access a key or array index, but 'at'-like
atTokenL :: Token -> AffineTraversal' Value (Maybe Value)
atTokenL = \case
  K k -> atKey k
  N n -> atNth n
