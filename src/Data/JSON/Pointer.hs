module Data.JSON.Pointer
  ( Pointer (..)
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)

-- TODO
newtype Pointer = Pointer Text
  deriving newtype (FromJSON)
