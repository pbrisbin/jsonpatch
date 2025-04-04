module Data.JSON.Patch.Apply
  ( applyPatches
  ) where

import Prelude

import Data.Aeson
import Data.JSON.Patch.Type

applyPatches :: [Patch] -> Value -> Either String Value
applyPatches _ = Right
