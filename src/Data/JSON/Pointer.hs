-- <https://datatracker.ietf.org/doc/html/rfc6901/>
module Data.JSON.Pointer
  ( Pointer (..)
  , Token (..)
  , tokensL
  , tokenL
  ) where

import Prelude

import Data.Aeson (FromJSON (..), Key, Value (..), withText)
import Data.Aeson.Optics
import Data.Text (Text)
import Optics

data Pointer
  = -- | @""@ means whole-document
    PointerEmpty
  | -- | @"/[.../]x"@ means path to a key or index @x@
    --
    -- NB. @"/"@ naturally becomes @'PointerPath' [] ('K' "")@
    PointerPath [Token] Token
  | -- | @"/[.../]-"@ means path to last element of an array
    PointerPathEnd [Token]

instance FromJSON Pointer where
  parseJSON = withText "Pointer" $ either fail pure . pointerFromText

pointerFromText :: Text -> Either String Pointer
pointerFromText = \case
  "/hello" -> Right $ PointerPath [] $ K "hello"
  "/baz" -> Right $ PointerPath [] $ K "baz"
  "/foo" -> Right $ PointerPath [] $ K "foo"
  x -> error $ show x -- TODO

data Token = K Key | N Int

tokensL :: [Token] -> AffineTraversal' Value Value
tokensL = foldr ((%) . tokenL) $ castOptic simple

tokenL :: Token -> AffineTraversal' Value Value
tokenL = \case
  K k -> key k
  N n -> nth n
