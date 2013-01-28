module Main where

import GA
import Radial
import Data.Random
import qualified Data.Foldable as F
import qualified Data.Sequence as S

ones :: S.Seq Bool -> R Double
ones ind = return $! fromIntegral $ F.sum $ fmap fromEnum ind

--main = runR (defaultGA ones) >>= print

values = [0..100]
target x = 3 * (x^2) + x + 5
inputs = map return values
outputs = map (return . target) values

main = do
  pop <- runR $ rbnGA defaultRBNParams (inputs, outputs)
  print $ errorOnCases (radialNeuronF (fittestIndividual pop)) inputs outputs

