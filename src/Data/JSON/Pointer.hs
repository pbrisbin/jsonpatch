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
import Data.Aeson (FromJSON (..), Key, Value (..), withText)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Optics
import Data.Attoparsec.Text
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
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
  either (\msg -> fail $ "Unable to read integer from " <> ds <> ": " <> msg) pure
    $ readEither ds

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

-- |
--
-- When traversing tokens, we can't use 'aTokenL' because we need to replace
-- missing bits with empty structures and use update semantics for 'Array's
atTokenL' :: Token -> AffineTraversal' Value Value
atTokenL' t = case t of
  K _ -> atTokenL t % non (Object mempty)
  N n -> atraversal (nMatcher n) (nUpdater n) % non (Array mempty)

-- | Update an index in a 'Value' using update semantics for arrays
nUpdater :: Int -> Value -> Maybe Value -> Value
nUpdater n nv = \case
  Just x | Array vec <- nv -> Array $ vSetAt n x vec
  v -> nInserter n nv v -- all other cases are the same

atTokenL :: Token -> AffineTraversal' Value (Maybe Value)
atTokenL = \case
  K k -> _Object % at k
  N n -> atraversal (nMatcher n) (nInserter n)

nMatcher :: Int -> Value -> Either Value (Maybe Value)
nMatcher n = \case
  Object km -> Right $ KeyMap.lookup (Key.fromString $ show n) km
  Array vec -> Right $ vec V.!? n
  v -> Left v

-- | Update an index in a 'Value' using insert semantics for arrays
nInserter :: Int -> Value -> Maybe Value -> Value
nInserter n nv = \case
  Nothing -> case nv of
    Object km -> Object $ KeyMap.delete (Key.fromString $ show n) km
    Array vec -> Array $ vDeleteAt n vec
    v -> v
  Just x -> case nv of
    Object km -> Object $ KeyMap.insert (Key.fromString $ show n) x km
    Array vec -> Array $ vInsertAt n x vec
    v -> v

-- | Delete from a vector and shift all later elements left
vDeleteAt :: Int -> Vector a -> Vector a
vDeleteAt n vec = vGenerate $ V.imapMaybe shift vec
 where
  shift idx a
    | idx < n = Just (idx, a)
    | idx == n = Nothing
    | otherwise = Just (idx - 1, a)

-- | Insert into a vector and shift all later elements right
vInsertAt :: Int -> a -> Vector a -> Vector a
vInsertAt n v vec = vGenerate $ V.imap shift vec <> pure (n, v)
 where
  shift idx a
    | idx >= n = (idx + 1, a)
    | otherwise = (idx, a)

-- | Update an index in a vector
vSetAt :: Int -> a -> Vector a -> Vector a
vSetAt n v vec = vGenerate $ V.imap set vec
 where
  set idx a
    | idx == n = (idx, v)
    | otherwise = (idx, a)

vGenerate :: Vector (Int, a) -> Vector a
vGenerate indexed = V.generate (V.length indexed) $ snd . (V.!) indexed
