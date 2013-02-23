module Crossover where

import qualified Data.Sequence as S
import Data.Random
import Data.Random.Distribution.Bernoulli
import Stochastic
import Utility

ifP p f a = do
  b <- sample stdUniform
  if b then f a else a
ifPM p f a = do
  b <- sample stdUniform
  if b then f a else return a

crossPair n a b = (top S.>< bottom', top' S.>< bottom) where
  (top, bottom) = S.splitAt n a
  (top', bottom') = S.splitAt n b

crossInds a b = do
  i <- sample (uniform 0 (S.length a - 1))
  return $! crossPair i a b
crossInds2 a b = crossInds a b >>= uncurry crossInds 

pairup seq = S.zip top bottom where
  (top, bottom) = S.splitAt middle seq
  middle = S.length seq `div` 2
unpair seq = fmap fst seq S.>< fmap snd seq
onPairs seq f = fmap unpair $ f $ pairup seq

onePointCrossover :: Double -> L (L a) -> R (L (L a))
onePointCrossover p pop = onPairs pop $ pmap p $ uncurry crossInds

twoPointCrossover :: Double -> L (L a) -> R (L (L a))
twoPointCrossover p pop = onPairs pop $ pmap p $ uncurry crossInds2

uniformCrossover :: Double -> L (L a) -> R (L (L a))
uniformCrossover p pop = onPairs pop $ pmap p $ uncurry uniformCross

seqUnzip seq = (fmap fst seq, fmap snd seq)
swapPair (a, b) = (b, a)
uniformCross :: L a -> L a -> R (L a, L a)
uniformCross ind ind' = do
  swapFuncs <- seqR (S.length ind) $ chooseBetween id swapPair 0.5
  return $ seqUnzip $ S.zipWith ($) swapFuncs (S.zip ind ind')


