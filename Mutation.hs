module Mutation where

import Control.Applicative
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Random.Extras as RE
import Data.Random
import Data.Bits
import qualified Data.Vector as V
import Stochastic
import Utility
import Data.Word
import Control.Lens


class Mutable a where
  mutate :: a -> R a

instance Mutable Bool where
  mutate b = return $! not b

instance Mutable Double where
  mutate r = (max 0 . min 1) <$> sample (normal r 1)

letters = V.fromList ['a'..'z'] 
lettersWithCaps = letters V.++ V.fromList ['A'..'Z']
alphaNums = letters V.++ V.fromList ['0'..'9']

data Symbol a = Symbol { symbol :: a, symbolVector :: (V.Vector a) } deriving (Eq, Show)

instance Mutable (Symbol a) where
  mutate (Symbol _ as) = do 
    n <- sample $ uniform 0 (V.length as - 1)
    return $ Symbol (as V.! n) as


--Point Mutation
pointMutateIndividual p ind = pmap p mutate ind
pointMutation :: (Mutable a) =>
  Double -> L (L a) -> R (L (L a))
pointMutation p d = T.traverse (pointMutateIndividual p) d


--BitSym for bit vectors < 32 bits
data BitSym = BitSym { bitSym :: Word32, bitsUsed :: Int } deriving (Show, Eq)

mutateBitSym p seq = let bits = bitsUsed $ seqHead seq in do
  indices <- genIndices p $ bits * S.length seq
  let indexFuncPairs = map (over _2 flipBit . splitIndex bits) indices
  return $! applyOver indexFuncPairs seq 

splitIndex bits i = (i `div` bits, i `mod` bits)
flipBit b (BitSym w n) = BitSym (xor (1 `shift` b) w) n

randomBitSymInd bits n = fmap ((\w -> BitSym w bits) . clearHighBits bits) <$> (seqR n $ sample stdUniform)
