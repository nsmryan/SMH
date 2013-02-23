module Selection where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Function
import Utility
import Control.Applicative
import Control.Monad
import Stochastic
import Data.Random

tournIndices :: Int -> Int -> R (L [Int])
tournIndices tournSize ps = seqR ps $ replicateM tournSize $ sample $ uniform 0 (ps - 1)

averageFitness pop = F.sum (fmap snd pop) / (fromIntegral $ S.length pop)
fittestIndividual pop = maximumByProj snd pop
maxBy f a b = if f a > f b then a else b

maximumByProj proj = F.maximumBy comp where comp a b = compare (proj a) (proj b)
tourny = fst . maximumByProj snd 
resolveIndices seq indices = map (S.index seq) indices
tournaments pop indices = fmap (tourny . resolveIndices pop) indices

tournamentSelection :: Int -> L (a, Double) -> R (L a)
tournamentSelection size pop = tournaments pop <$> tournIndices size (S.length pop)

rangeFuncs :: L (a, Double) -> L (a, Double -> Bool)
rangeFuncs pop = S.zip inds $ (S.zipWith inRange summed fitnesses) where
  summed = S.scanl (+) 0 fitnesses
  inRange top fit fit' = fit' < top && fit' >= fit 
  (inds, fitnesses) = (fmap fst pop, fmap snd pop)

rankSelection :: L (a, Double) -> R (L a)
rankSelection pop = do
  let choices = fmap fromIntegral $ S.mapWithIndex const $ S.sortBy (compare `on` snd) pop
  return $ rouletteWheel' pop choices

stochasticUniform :: L (a, Double) -> R (L a)
stochasticUniform pop = let
  popLen = S.length pop
  avgFitness = (F.sum $ (snd <$> pop)) / fromIntegral popLen in do
    choice <- sample $ uniform 0 (avgFitness - 1)
    let choices = choice S.<| S.replicate (popLen-1) avgFitness
    return $ rouletteWheel' pop choices 

rouletteWheel :: L (a, Double) -> R (L a)
rouletteWheel pop = let totalFitness = F.sum $ (snd <$> pop) in do
    choices <- samplesInRange totalFitness $ S.length pop
    return $ rouletteWheel' pop choices

rouletteWheel' :: L (a, Double) -> L Double -> L a
rouletteWheel' pop points = fmap (fst . searchIn indexTests) points where
  indexTests = rangeFuncs pop
  searchIn pop' point = let
    middleIndex = ceiling $ (/ 2.0) $ fromIntegral $ S.length pop'
    middleInd = pop' `S.index` middleIndex in
      case (snd middleInd) point of
        True -> middleInd
        False -> if S.length pop == middleIndex 
          then error "empty partition in rouletteWheel'"
          else searchIn (S.drop (middleIndex + 1) pop') point
      

kelitism 0 (elites, pop) = return ([], pop)

kelitism 1 ([], pop) = return ([fittestIndividual pop], pop) where
kelitism 1 ((elite:[]), pop) = return ([elite'], elite' S.<| S.drop 1 pop) where
  bestInd = fittestIndividual pop
  elite' = maxBy snd bestInd elite
  
kelitism k (elites, pop) = undefined
