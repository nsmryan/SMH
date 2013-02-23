module Utility where

import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.Bits

type L a = S.Seq a

seqR n m = T.sequence $ S.replicate n m 

seqHead = flip S.index 0
seqTail = S.drop 1

clearHighBits bits w = w .&. ((2 ^ bits) - 1)
