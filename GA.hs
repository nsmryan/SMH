{-# LANGUAGE FlexibleContexts #-}

module GA where

import Prelude hiding ((.), id)

import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Foldable as F

import Data.Random
import qualified Data.Random.Extras as RE
import Data.Random.Distribution.Normal
import qualified Data.Vector as V

import Data.List.Split

import Control.Arrow
import Control.Category
import Control.Applicative
import Control.Monad

import Mutation
import Selection
import Crossover
import Evaluation
import Utility
import Stochastic 


data GAParams = GAParams { pm   :: Double,
                           pc   :: Double,
                           ps   :: Int,
                           is   :: Int,
                           gens :: Int,
                           ts   :: Int}

defaultGAParams = GAParams { pm = 0.01,
                             pc = 0.6,
                             ps = 50,
                             is = 20,
                             gens = 100,
                             ts = 2 }


--Replace with streams.
simpleGAFunction fitness pm pc ps is gens ts randInd =
    randomPopulation ps is randInd >>= runNTimes gens genFunc where
      genFunc = pointMutation pm >=> onePointCrossover pc >=> evaluate fitness >=> tournamentSelection ts

simpleGA :: (Mutable a) =>
  (L a -> R Double) -> GAParams -> (Int -> R (L a)) -> R (L (L a))
simpleGA fitness (GAParams pm pc ps is gens ts) randInd =
  simpleGAFunction fitness pm pc ps is gens ts randInd

defaultGA :: (Mutable a, Distribution StdUniform a) =>
             (L a -> R Double) -> R (L (L a)) 
defaultGA fitness = simpleGA fitness defaultGAParams randomIndividual

minToMax fitness = 1 / if fitness == 0.0 then 0.000001 else fitness

symToStr sym = F.toList $ fmap symbol sym
strMatches str str' = fromIntegral $ sum $ map fromEnum $ zipWith (==) str $ symToStr str'
randomSymbolInd syms size = seqR size $ Symbol <$> sym <*> symVect where
  sym = sample $ RE.choice syms
  symVect = return $ V.fromList syms
evolveSentence sentence = simpleGA (return . (strMatches sentence)) gaparams randInd where
  gaparams = defaultGAParams { is = length sentence, ps = 100, gens = 1000, pm = 0.02}
  randInd n = seqR n $ (mutate (Symbol 'a' alphaNums))

{-
boolGA intGA realGA symbolGA
-}

