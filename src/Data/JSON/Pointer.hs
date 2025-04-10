-- <https://datatracker.ietf.org/doc/html/rfc6901/>
module Data.JSON.Pointer
  ( Pointer (..)
  , pointerFromText
  , pointerToText
  , pointerToString
  , pointerL
  , atPointerL
  , splitPointer
  ) where

import Prelude

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Value, withText)
import Data.Aeson.Optics (_JSON)
import Data.Attoparsec.Text
import Data.JSON.Pointer.Token
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Optics

newtype Pointer = Pointer
  { tokens :: [Token]
  }
  deriving stock (Eq, Show)

instance FromJSON Pointer where
  parseJSON = withText "Pointer" $ either fail pure . pointerFromText

pointerFromText :: Text -> Either String Pointer
pointerFromText = parseOnly pointerP

pointerP :: Parser Pointer
pointerP = do
  ts <- (char '/' *> tokenP `sepBy1` char '/' <|> pure []) <* endOfInput
  pure $ Pointer ts

pointerToText :: Pointer -> Text
pointerToText = ("/" <>) . T.intercalate "/" . map tokenToText . (.tokens)

pointerToString :: Pointer -> String
pointerToString = unpack . pointerToText

-- | Access a 'Pointer' like 'ix', used for indexing
pointerL :: Pointer -> AffineTraversal' Value Value
pointerL = foldr ((%) . tokenL) (castOptic simple) . (.tokens)

-- | Access a 'Pointer' like 'at', used for adding or removing
atPointerL :: Pointer -> AffineTraversal' Value (Maybe Value)
atPointerL p = case splitPointer p of
  Nothing -> castOptic _JSON -- hack to return as-is
  Just (parent, t) -> pointerL parent % atTokenL t

splitPointer :: Pointer -> Maybe (Pointer, Token)
splitPointer p = go <$> nonEmpty p.tokens
 where
  go x = (Pointer $ NE.init x, NE.last x)
