module Data.JSON.Patch.Prelude
  ( module X
  , note
  ) where

import Prelude as X

import Control.Applicative as X (asum, (<|>))
import Control.Exception as X (Exception (..))
import Control.Monad as X (foldM, unless, void, when)
import Data.Bifunctor as X (bimap, first, second)
import Data.List.NonEmpty as X (NonEmpty, nonEmpty)
import Data.Maybe as X (fromMaybe)
import Data.Text as X (Text, pack, unpack)
import Data.Vector as X (Vector)
import GHC.Generics as X (Generic)

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
