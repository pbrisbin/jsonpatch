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
  , atTokenL
  ) where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson (FromJSON (..), Key, Value (..), withText)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Optics
import Data.Aeson.Optics.Ext
import Data.Attoparsec.Text
import Data.List (isPrefixOf)
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
  t -> parseOnly (pointerP <* endOfInput) t

pointerP :: Parser Pointer
pointerP = do
  ts <- char '/' *> many' (tokenP <* char '/')
  PointerPathEnd ts <$ char '-' <|> PointerPath ts <$> tokenP

tokenP :: Parser Token
tokenP = N <$> indexP <|> K <$> keyP

keyP :: Parser Key
keyP =
  Key.fromText
    . T.replace "~0" "~"
    . T.replace "~1" "/"
    <$> takeTill (== '/')

indexP :: Parser Int
indexP = do
  ds <- many1 digit

  -- readEither is lax about this in particular
  when ("0" `isPrefixOf` ds && length ds > 1) $ do
    err ds "cannot contain leading zeros"

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
tokensL = foldr ((%) . atTokenL') $ castOptic simple

atTokenL' :: Token -> AffineTraversal' Value Value
atTokenL' t = case t of
  K k -> key k
  N n -> nth n

atTokenL :: Token -> AffineTraversal' Value (Maybe Value)
atTokenL = \case
  K k -> atKey k
  N n -> atNth' n
