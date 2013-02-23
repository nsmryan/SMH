{-# LANGUAGE FlexibleContexts #-}

module Stochastic where

import qualified Data.Stream as STR
import Data.Random
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Traversable as T
import qualified Data.DList as D
import qualified Data.Sequence as S
import Control.Applicative
import Data.Random.Distribution.Bernoulli
import Data.Random.Source.PureMT
import Utility

type R a = StateT PureMT (Writer (D.DList String)) a

writeLog :: String -> R ()
writeLog str = tell $ D.singleton str

runR :: R a -> IO a
runR r = do
  g <- newPureMT
  let (result, log) = runWriter $ evalStateT r g 
  putStrLn $ unlines $ D.toList log
  return result

pmap p f seq = do
  indices <- genIndices p $ S.length seq
  let indexFuncPairs = zip indices $ repeat f
  applyOverM indexFuncPairs seq 
pfmap p f f' d = T.sequence $ fmap func d where
  func a = do
    b <- sample $ bernoulli p
    if b then f a else f' a

uniToGeo p u = floor $! log u / log (1 - p)
geoDist p = fmap (uniToGeo p) $! sample stdUniform
genIndices :: Double -> Int -> R [Int]
genIndices p maxN = boundIndices 0 [] where
  boundIndices acc indices = do
    index <- geoDist p
    let indexSum = acc + index in
      case indexSum > maxN of
      True -> return indices
      False -> boundIndices indexSum (index : indices)

applyOver [] seq = seq
applyOver ((0, f) : pairs) seq = applyOver pairs $ f (seqHead seq) S.<| seqTail seq
applyOver ((i, f) : pairs) seq
  | S.null seq = S.empty
  | otherwise = front S.>< applyOver pairs bottom where
    (top, bottom) = S.splitAt i seq
    front = case S.null top of
      True -> S.empty
      False -> f (S.index top 0) S.<| S.drop 1 top
applyOverM :: (Monad m, Applicative m) => [(Int, a -> m a)] -> L a -> m (L a)
applyOverM [] seq = return seq
applyOverM ((0, f) : pairs) seq = ((S.<| (seqTail seq)) <$> f (seqHead seq)) >>= applyOverM pairs
applyOverM ((i, f) : pairs) seq
  | S.null seq = return S.empty
  | otherwise = do
    let (top, bottom) = S.splitAt i seq
    front <- case S.null top of
      True -> return S.empty
      False -> (S.<|) <$> (f (S.index top 0)) <*> return (seqTail top)
    (front S.><) <$> applyOverM pairs bottom

samplesInRange top n = seqR n $ (sample $ uniform 0 (top - 1))

runNTimes :: Int -> (a -> R a) -> a -> R a
runNTimes n fm = foldl (>=>) return $ STR.take n $ STR.repeat fm

chooseBetween :: a -> a -> Double -> R a
chooseBetween a b p = do
  bool <- sample $ bernoulli p
  return $ if bool then a else b

randomIndividual :: (Distribution StdUniform a) => Int -> R (L a)
randomIndividual indSize = sample $ seqR indSize stdUniform

randomPopulation :: Int -> Int -> (Int -> R (L a)) -> R (L (L a))
randomPopulation popSize indSize randInd = seqR popSize $ randInd indSize

defaultRandomPopulation ps is = randomPopulation ps is randomIndividual

