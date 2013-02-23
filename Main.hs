module Main where

import GA
import Data.Random
import qualified Data.Foldable as F
import qualified Data.Sequence as S

ones :: S.Seq Bool -> R Double
ones ind = return $! fromIntegral $ F.sum $ fmap fromEnum ind

--main = runR (defaultGA ones) >>= print

--values = [0..100]
--target x = 3 * (x^2) + x + 5
--inputs = map return values
--outputs = map (return . target) values

main = do
  let sentence = "methinks it is like a weasel"
  pop <- runR $ evolveSentence sentence
  pop' <- runR $ evaluate (strMatches sentence) pop
  print $ fittestIndividual pop'

