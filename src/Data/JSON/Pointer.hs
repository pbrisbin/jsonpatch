-- <https://datatracker.ietf.org/doc/html/rfc6901/>
module Data.JSON.Pointer
  ( Pointer (..)
  , Token (..)
  , tokensL
  , tokenL
  ) where

import Prelude

import Control.Lens
  ( Lens'
  , Setter'
  , Traversal'
  , lens
  , to
  , (&)
  , (.~)
  , (^.)
  , _Just
  )
import Data.Aeson (FromJSON (..), Key, Value (..), withText)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (key, nth)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

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
pointerFromText = undefined

data Token = K Key | N Int

tokensL :: [Token] -> Traversal' Value Value
tokensL = _ . map tokenL

tokenL :: Token -> Traversal' Value Value
tokenL = \case
  K k -> key k
  N n -> nth n
