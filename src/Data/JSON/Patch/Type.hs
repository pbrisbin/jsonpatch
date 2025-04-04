module Data.JSON.Patch.Type
  ( Patch (..)
  ) where

import Prelude

import Data.Aeson

data Patch = Patch

instance FromJSON Patch where
  parseJSON = undefined
