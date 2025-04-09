-- <https://datatracker.ietf.org/doc/html/rfc6901/>
module Data.JSON.Pointer
  ( Pointer (..)
  , pointerFromText
  , pointerToText
  , pointerToString
  ) where

import Prelude

import Data.Aeson (FromJSON (..), withText)
import Data.Attoparsec.Text
import Data.JSON.Pointer.Token
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, unpack)
import Data.Text qualified as T

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

pointerToText :: Pointer -> Text
pointerToText = \case
  PointerEmpty -> ""
  PointerPath ts t -> tokensToText $ ts <> [t]
  PointerPathEnd ts -> tokensToText ts <> "/-"

pointerToString :: Pointer -> String
pointerToString = unpack . pointerToText

pointerPathP :: Parser Pointer
pointerPathP = do
  ts <- maybe (fail "") pure . nonEmpty =<< pointerP
  pure $ PointerPath (NE.init ts) $ NE.last ts

pointerPathEndP :: Parser Pointer
pointerPathEndP = PointerPathEnd <$> pointerP

pointerP :: Parser [Token]
pointerP = char '/' *> tokenP `sepBy1` char '/' <* endOfInput
