-- |
--
-- Module      : Data.JSON.Patch
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Data.JSON.Patch
  ( Patch
  , PatchError
  , applyPatches
  ) where

import Data.JSON.Patch.Apply
import Data.JSON.Patch.Type
