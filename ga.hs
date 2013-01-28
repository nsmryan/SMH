{-# LANGUAGE FlexibleContexts #-}

module GA where


import Prelude hiding ((.), id)

import qualified Data.DList as D
import qualified Data.Sequence as S
import qualified Data.Stream as STR
import qualified Data.Traversable as T
import qualified Data.Foldable as F

import Data.Random
import qualified Data.Random.Extras as RE
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Normal
import Data.Random.Source.PureMT

import Data.List.Split

import Control.Arrow
import Control.Category
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Control.DeepSeq


class PMap d where
  pmap :: Double -> (a -> R a) -> d a -> R (d a)
  pmap p f d = pfmap p f return d
  pfmap :: Double -> (a -> R b) -> (a -> R b) -> d a -> R (d b)

instance PMap S.Seq where
  pmap p f seq = do
    indices <- genIndices p $ S.length seq
    let indexFuncPairs = zip indices $ repeat f
    applyOverM indexFuncPairs seq 
  pfmap p f f' d = T.sequence $ fmap func d where
    func a = do
      b <- sample $ bernoulli p
      if b then f a else f' a


class Mutable a where
  mutate :: a -> R a

instance Mutable Bool where
  mutate b = return $! not b

instance Mutable Double where
  mutate r = (max 0 . min 1) <$> sample (normal r 1)

instance Mutable Char where
  mutate c = sample $ RE.choice ['a'..'z']


type L a = S.Seq a

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

uniToGeo p u = floor $ log u / log (1 - p)
geoDist p = fmap (uniToGeo p) $ sample stdUniform
genIndices :: Double -> Int -> R [Int]
genIndices p maxN = boundIndices 0 [] where
  boundIndices acc indices = do
    index <- geoDist p
    let indexSum = acc + index in
      case indexSum > maxN of
      True -> return indices
      False -> boundIndices indexSum (index : indices)

applyOver [] seq = seq
applyOver ((i, f) : pairs) seq = front S.>< applyOver pairs bottom where
  (top, bottom) = S.splitAt i seq
  front = case S.null top of
    True -> S.empty
    False -> f (S.index top 0) S.<| S.drop 1 top
applyOverM [] seq = return seq
applyOverM ((i, f) : pairs) seq = do
  let (top, bottom) = S.splitAt i seq
  front <- case S.null top of
    True -> return S.empty
    False -> (S.<|) <$> (f (S.index top 0)) <*> return (S.drop 1 top)
  (front S.><) <$> applyOverM pairs bottom

pointMutation :: (T.Traversable f, PMap d, Mutable a) =>
  Double -> f (d a) -> R (f (d a))
pointMutation p d = T.traverse (pmap p mutate) d

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

pairup seq = S.zip top bottom where
  (top, bottom) = S.splitAt middle seq
  middle = S.length seq `div` 2
unpair seq = fmap fst seq S.>< fmap snd seq
onPairs seq f = fmap unpair $ f $ pairup seq

crossover :: Double -> L (L a) -> R (L (L a))
crossover p pop = onPairs pop $ pmap p (uncurry crossInds)


tournIndices :: Int -> Int -> R (L [Int])
tournIndices tournSize ps = seqR ps $ replicateM tournSize $ sample $ uniform 0 (ps - 1)

maximumByProj proj = F.maximumBy comp where comp a b = compare (proj a) (proj b)
tourny = fst . maximumByProj snd 
resolveIndices seq indices = map (S.index seq) indices
tournaments pop indices = fmap (tourny . resolveIndices pop) indices

tournamentSelection :: Int -> L (a, Double) -> R (L a)
tournamentSelection size pop = tournaments pop <$> tournIndices size (S.length pop)

evaluate :: (a -> R Double) -> L a -> R (L (a, Double))
evaluate fitness pop = fmap (S.zip pop) $ T.sequence $ fmap fitness pop

runNTimes :: Int -> (a -> R a) -> a -> R a
runNTimes n fm = foldl (>=>) return $ STR.take n $ STR.repeat fm

randomPopulation :: (Distribution StdUniform a) =>
  Int -> Int -> R (L (L a))
randomPopulation popSize indSize = sample $ seqR popSize $ seqR indSize stdUniform

seqR n m = T.sequence $ S.replicate n m 

averageFitness pop = F.sum (fmap snd pop) / (fromIntegral $ S.length pop)
fittestIndividual pop = maximumByProj snd pop
maxBy f a b = if f a > f b then a else b

kelitism 0 (elites, pop) = return ([], pop)

kelitism 1 ([], pop) = return ([fittestIndividual pop], pop) where
kelitism 1 ((elite:[]), pop) = return ([elite'], elite' S.<| S.drop 1 pop) where
  bestInd = fittestIndividual pop
  elite' = maxBy snd bestInd elite
  
kelitism k (elites, pop) = undefined

--Replace with streams.
simpleGAFunction fitness pm pc ps is gens ts =
    randomPopulation ps is >>= runNTimes gens genFunc where
      genFunc = pointMutation pm >=> crossover pc >=> evaluate fitness >=> tournamentSelection ts

simpleGA :: (Mutable a, Distribution StdUniform a) =>
  (L a -> R Double) -> GAParams -> R (L (L a))
simpleGA fitness (GAParams pm pc ps is gens ts) = simpleGAFunction fitness pm pc ps is gens ts

defaultGA :: (Mutable a, Distribution StdUniform a) =>
             (L a -> R Double) -> R (L (L a))
defaultGA fitness = simpleGA fitness defaultGAParams

minToMax fitness = 1 / if fitness == 0.0 then 0.000001 else fitness

--Neural Netwworks
data Neuron a = Neuron { activationF :: a -> a,
                         acceptInputs :: [a] -> [a] -> a,
                         bias :: a,
                         weights :: [a] }
applyNeuron (Neuron activation acceptIns bias weights) ins = activation $ acceptIns weights (bias:ins)

layerF neurons inputs = zipWith ($) neurons $ replicate (length neurons) inputs

--consider making an ANNGAParams type with arch type, no ind length, and other options.
annGA :: ([Double] -> Neuron Double) -> 
         ([[Double]], [[Double]]) ->
         [Int] ->
         GAParams ->
         R (L (L Double))
annGA neuron trainingSet@(inputs, outputs) arch gaparams = simpleGA fitness gaparams where
  fitness = return . minToMax . annFitness neuron arch trainingSet

weightListToNetwork _ [] weights = []
weightListToNetwork _ (_:[]) weights = []
weightListToNetwork neuron (ins:outs:rest) weights = layer : weightListToNetwork neuron (outs:rest) weights' where
  layerSize = outs + (ins * outs)
  (layerWeights, weights') = splitAt layerSize weights
  layer = map neuron $ chunksOf (1 + ins) layerWeights

annToF :: [[Neuron Double]] -> [Double] -> [Double]
annToF [] inputs = inputs
annToF (layer:layers) input = layers `annToF` layerOutput where
  layerOutput = map ($ inputWithBias) (map applyNeuron layer) 
  inputWithBias = 1 : input

annNetworkSize [] = 0
annNetworkSize arch@(_:rest) = (sum rest) + sum ((*) <$> init arch <*> rest)

annFitness neuron arch (inputs, outputs) weights = errorOnCases network inputs outputs where
  network = annToF $ weightListToNetwork neuron arch $ F.toList weights
errorOnCases f ins outs = sum $ zipWith (errorOn f) ins outs
errorOn f ins outs = sum $ map (^2) $ zipWith (-) (f ins) outs 
sigmoid r = 1.0 / (1.0 + exp (-r))

strMatches str str' = return $ fromIntegral $ sum $ map fromEnum $ zipWith (==) str $ F.toList str'
evolveSentence sentence = simpleGA (strMatches sentence) $
  defaultGAParams { is = length sentence }

{-
simple cases and defaults for boolGA intGA realGA symbolGA
-}

type R a = StateT PureMT (Writer (D.DList String)) a

writeLog :: String -> R ()
writeLog str = tell $ D.singleton str

runR :: R a -> IO a
runR r = do
  g <- newPureMT
  let (result, log) = runWriter $ evalStateT r g 
  putStrLn $ unlines $ D.toList log
  return result

