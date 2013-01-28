module Radial where

import Prelude hiding ((.), id)
import GA
import Data.Random
import Data.Monoid
import qualified Data.Random.Extras as RE
import Data.List.Split
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Traversable as T


data RBNParams = RBNParams { rbnPMActive      :: Double,
                             rbnPMPoints      :: Double,
                             rbnPMBeta        :: Double,
                             rbnPMSigma       :: Double,
                             rbnPC            :: Double,
                             rbnPS            :: Int,
                             rbnHiddenNeurons :: Int,
                             rbnGens          :: Int,
                             rbnTS            :: Int } deriving (Show)

data RadialNeuron = RadialNeuron { rbnBeta   :: Double
                                 , rbnPoints :: [Point]
                                 , rbnPointA :: Point
                                 , rbnPointB :: Point
                                 , rbnActive :: Bool } deriving (Show)

type Point = [Double]

type RBNInd = (L RadialNeuron, L Double)

defaultRBNParams = RBNParams { rbnPMActive = 0.001,
                               rbnPMPoints = 0.005,
                               rbnPMBeta = 0.005,
                               rbnPMSigma = 0.001,
                               rbnPC = 0.6,
                               rbnPS = 50,
                               rbnHiddenNeurons = 5,
                               rbnGens = 100,
                               rbnTS = 2 }

setRbnBeta   neuron beta   = neuron { rbnBeta = beta } 
setRbnPoints neuron points = neuron { rbnPoints = points }
setRbnPointA neuron pointA = neuron { rbnPointA = pointA }
setRbnPointB neuron pointB = neuron { rbnPointB = pointB }
setRbnActive neuron active = neuron { rbnActive = active }

rbnCross :: RBNInd -> RBNInd -> R (RBNInd, RBNInd)
rbnCross (a, b) (a', b') = let len = S.length a + S.length b in do
  i <- sample $ uniform 0 (len - 1)
  return $ case i < S.length a of
    True -> let (ac, ac') = crossPair i a a' in ((ac, b), (ac', b'))
    False -> let (bc, bc') = crossPair i b b' in ((a, bc), (a', bc'))

fstM :: (Applicative m, Monad m) => (a -> m c) -> (a, b) -> m (c, b)
fstM f (first, second) = (,) <$> f first <*> return second
sndM :: (Applicative m, Monad m) => (b -> m c) -> (a, b) -> m (a, c)
sndM f (first, second) = (,) <$> return first <*> f second

rbnCrossover :: Double -> L RBNInd -> R (L RBNInd)
rbnCrossover p pop = onPairs pop $ pmap p $ uncurry rbnCross

seqHead seq = S.index seq 0
seqChunks size seq = let (front, back) = S.splitAt size seq in
  case S.null front of
    True -> S.singleton back
    False -> front S.<| seqChunks size back

mutateSigma :: Double -> L RBNInd -> R (L RBNInd)
mutateSigma pmS pop = T.traverse (sndM (pmap pmS mutate)) pop where

mutateBeta :: Double -> L RBNInd -> R (L RBNInd)
mutateBeta pmB pop = T.traverse (fstM (pmap pmB betaMutate)) pop where
  betaMutate neuron = mutate (rbnBeta neuron) >>= return . (\beta' -> neuron {rbnBeta = beta'})

mutatePoints :: Double -> L RBNInd -> R (L RBNInd)
mutatePoints pmP pop = T.traverse (fstM (pmap pmP pointMutate)) pop where
  pointMutate neuron = do
    b <- sample stdUniform 
    point <- sample $ RE.choice $ rbnPoints neuron
    return $ if b
      then neuron {rbnPointA = point}
      else neuron {rbnPointB = point}

mutateActive :: Double -> L RBNInd -> R (L RBNInd)
mutateActive pmA pop = T.traverse (fstM (pmap pmA activeMutate)) pop where
  activeMutate neuron = mutate (rbnActive neuron) >>= return . (\active' -> neuron {rbnActive = active'})

rbnMutation pmA pmP pmB pmS = mutateSigma pmS >=> mutateBeta pmB >=> mutatePoints pmP >=> mutateActive pmA

randomRBNRadial points = RadialNeuron <$> beta <*> return points <*> pA <*> pB <*> active where
  beta = sample stdUniform 
  pA = sample $ RE.choice points 
  pB = sample $ RE.choice points 
  active = sample stdUniform
randomRBNSigma :: Int -> Int -> R (L Double)
randomRBNSigma numOutputs numRadials = seqR (numOutputs * numRadials) $ sample stdUniform
randomRBNInd :: Int -> Int -> [Point] -> R RBNInd
randomRBNInd hs os points = (,) <$> (seqR hs $ randomRBNRadial points) <*> (randomRBNSigma hs os)
randomRBNPop :: Int -> Int -> Int -> [Point] -> R (L RBNInd)
randomRBNPop is hs os points = seqR is $ randomRBNInd hs os points

radialNeuronF (radials, sigmas) inputs = let
  radialFunc = radialsF radials
  numRadials = length $ filter rbnActive $ F.toList radials
  sigmaFunc = sigmasF numRadials sigmas in
  sigmaFunc $ radialFunc inputs

sigmasF :: Int -> L Double -> Point -> Point
sigmasF numRadials sigmaNeurons inputs = layerF (map sigmaF sigList) inputs where
  sigList = F.toList $ seqChunks numRadials sigmaNeurons
sigmaF sigmaNeuron inputs = sum $ zipWith (*) inputs $ F.toList sigmaNeuron

radialsF :: L RadialNeuron -> Point -> Point
radialsF radialNeurons inputs = layerF (map radialF radialList) inputs where radialList = F.toList radialNeurons
radialF (RadialNeuron beta points pointA pointB active) inputs = let cent = center pointA pointB in
  gaussian beta $ distance cent inputs
distance center point = sqrt $ sum $ map (^2) $ zipWith (-) center point
center pA pB = zipWith avg pA pB where avg a b = (a + b) / 2
gaussian beta x = exp $ (negate beta) * (x ^ 2)

rbnGA (RBNParams pmA pmP pmB pmS pc ps hn gens ts) (inputs, outputs) =
  randomRBNPop ps hn numOutputs inputs >>= runNTimes gens genFunc where
    genFunc = rbnMutation pmA pmP pmB pmS >=> rbnCrossover pc >=> evaluate fitness >=> tournamentSelection ts
    fitness ind = return $ minToMax $ errorOnCases (radialNeuronF ind) inputs outputs
    numInputs = length $ head inputs
    numOutputs = length $ head outputs

