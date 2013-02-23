module Evaluation where

import Stochastic
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Random
import Utility

evaluate :: (a -> R Double) -> L a -> R (L (a, Double))
evaluate fitness pop = fmap (S.zip pop) $ T.sequence $ fmap fitness pop

