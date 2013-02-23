{-# LANGUAGE BangPatterns #-}
module RGEP where

import Prelude hiding ((.), id)

import Data.Bits
import Data.Maybe
import Data.Word
import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Random.Source.PureMT
import Data.Random.Distribution.Exponential

import Control.Monad
import Control.Monad.State
import Control.Category
import Control.Applicative
import Control.Arrow
import Control.Lens

import Math.Polynomial
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJ
import Text.Printf

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Traversable as T

import Mutation
import Crossover
import Utility
import Stochastic
import Selection
import GA

data RGEPParams = RGEPParams
  { pm :: Double,
    pc1 :: Double,
    pc2 :: Double,
    pr :: Double,
    ps :: Int,
    is :: Int,
    gens :: Int,
    elites :: Int,
    ts :: Int }

--defaultRGEPParams = RGEPParams 0.001 0.6 0.6 0.6 10 10 10 1 2
defaultRGEPParams = RGEPParams 0.005 0.6 0.6 0.6 100 40 1000 1 2

type Sym = String
type Env a b = M.Map a b
type StackOp a = [a] -> [a]

rgepSymMap :: [Sym] -> [Sym] -> M.Map Word32 Sym
rgepSymMap terms ops = M.fromList $ zip allWord32s $ map (word32ToSym terms ops) allWord32s where
  allWord32s = map toEnum $ [0..(2 ^ rgepBitsNeeded ops terms) - 1]

word32ToSym terms ops w = let index = fromIntegral $! (w  `shift` (-1)) in
  if 0 == (w .&. 1)
    then terms !! (index `mod` length terms)
    else ops !! (index `mod` length ops)
 
rgepEval :: L (StackOp a) -> Maybe a
rgepEval fs = let
  stackFunc = F.foldl (.) id fs in
    case stackFunc [] of
      [] -> Nothing
      (a:as) -> return a

terminal = (:)
operator1 op (a:as) = op a : as
operator1 _ as = as
operator2 op (a:a':as) = a `op` a' : as
operator2 _ as = as
operator3 op (a:a':a'':as) = op a a' a'' : as
operator3 _ as = as 
dup (a:as) = a:a:as
dup as = as
swap (a:a':as) = a':a:as
swap as = as
nip (a:a':as) = a:as
nip as = as
overOp (a:a':as) = a':a:a':as
overOp as = as
stackCombinators = [("dup", dup), ("drop", drop 1), ("swap", swap), ("nip", nip), ("over", overOp)]

--Extends a term language with variables and let expressions
--remember to interpret unbound variables as id :: [a]->[a]
data TermLang expr = Var Sym
                   | Expression expr
                   | LetExpr Sym (TermLang expr) (TermLang expr)

rgepExpress :: 
  a             ->  -- Empty value
  M.Map Word32 (StackOp a) ->
  L BitSym ->
  a
rgepExpress empty mapping ind = evalInd ind where
  findFrom (BitSym k _) = case M.lookup k mapping of
    Nothing -> error "Unexpected symbol"
    Just a -> a
  evalInd = maybe empty id . rgepEval . fmap findFrom

--rgepMutation ::
--  Double -> Int -> L (L Word32) -> R (L (L Word32))
--rgepMutation p bitsUsed pop = T.traverse mutateInd pop where
--  mutateInd ind = flipWithP bitsUsed p ind

rgepBitsNeeded :: [a] -> [a] -> Int
rgepBitsNeeded terms ops = 1 + max (bits terms) (bits ops) where
  bits (_ : []) = 1
  bits as = ceiling $ logBase 2.0 $ fromIntegral $ length as

randomRGEPPopulation ::
  Int -> Int -> Int -> R (L (L BitSym))
randomRGEPPopulation bitsUsed ps is = randomPopulation ps is $ randomBitSymInd bitsUsed

simpleRGEPFunction terms ops empty fitness pm pc1 pc2 pr ps is gens elites ts = rgep where
  genFunc = traverseOf _2 (traverse (mutateBitSym pm) >=>
                           onePointCrossover pc1      >=>
                           twoPointCrossover pc2      >=>
                           evalRGEP) >=>
            kelitism 1        >=>
            traverseOf _2 (tournamentSelection ts)
  expressInd = rgepExpress empty bitsToSymMap
  symToOps = terms ++ ops
  bitsToSymMap = M.map (fromJust . flip lookup symToOps) $ rgepSymMap (map fst terms) (map fst ops)
  bitsUsed = rgepBitsNeeded terms ops
  evalRGEP pop = do
    fitnesses <- T.traverse fitness $ fmap expressInd pop
    let evaledPop = S.zip pop fitnesses in do
      writeLog $ printf "best: %.4f, avg: %.4f" (snd $ fittestIndividual $ evaledPop) (averageFitness evaledPop)
      return evaledPop
  rgep = do 
    writeLog $ printf "bits  used: %d" bitsUsed
    writeLog $ printf "pm: %.4f pc1: %.4f pc2: %.4f pr: %.4f " pm pc1 pc2 pr
    writeLog $ printf "population size: %d individual size: %d" ps is
    writeLog $ printf "elites: %d" elites
    writeLog $ printf "%d generations" gens 
    initialPopulation <- randomRGEPPopulation bitsUsed ps is 
    (((elite, bestFitness):[]), finalPopulation) <- runNTimes gens genFunc ([], initialPopulation)
    writeLog $ printf "winner is: %s" (show $ expressInd elite)
    writeLog $ printf "with fitness %s" (show bestFitness)
    return $! (fmap expressInd finalPopulation, (expressInd elite, bestFitness))

simpleRGEP :: (Show a) => 
  [(Sym, ([a] -> [a]))] ->  -- Terminal mappings
  [(Sym, ([a] -> [a]))] ->  -- Operator mappings
  a                     ->  -- Default value
  (a -> R Double)       ->  -- Fitness evaluator
  RGEPParams            ->  -- Algorithm parameters
  R (L a, (a, Double))      -- Population of solutions
simpleRGEP terms ops empty fitness (RGEPParams pm pc1 pc2 pr ps is gens elites ts) = 
  simpleRGEPFunction terms ops empty fitness pm pc1 pc2 pr ps is gens elites ts 

defaultRGEP :: (Show a) =>
  [(Sym, ([a] -> [a]))] ->  -- Terminal mappings
  [(Sym, ([a] -> [a]))] ->  -- Operator mappings
  a                     ->  -- Default value
  (a -> R Double)       -> 
  R (L a, (a, Double))
defaultRGEP ops terms empty fitness = simpleRGEP ops terms empty fitness defaultRGEPParams

type Polynomial = Poly Double
polyFitness testCases poly = return $! minToMax $! sum $! zipWith squaredDiff inputs outputs where
  squaredDiff a b = (a - b) ^ 2
  f = evalPoly poly
  inputs = map fst testCases
  outputs = map snd testCases
values = [0..100]
target !x = 3 * (x^2) + x + 5

functionRGEP :: [(Double, Double)] -> R (L Polynomial)
functionRGEP testCases = let
    ops = [("+", operator2 addPoly), ("*", operator2 multPoly)]
    terms = [("x", terminal x),
             ("0", terminal (constPoly 0)),
             ("1", terminal (constPoly 1)),
             ("2", terminal (constPoly 2)),
             ("5", terminal (constPoly 5))]
    empty = constPoly 0 in do
  (pop, (elite, fit)) <- defaultRGEP ops terms empty $ polyFitness testCases
  writeLog $ "best ind: " ++ show elite
  writeLog $ "with fitness: " ++ show fit
  return pop

data ArithExpr = ArithVar Sym
               | ArithConst Double
               | ArithPlus ArithExpr ArithExpr
               | ArithDiv ArithExpr ArithExpr
               | ArithMult ArithExpr ArithExpr
               | ArithPow ArithExpr ArithExpr
               | ArithSqrt ArithExpr
               | ArithNeg ArithExpr

arithOps = [
            ("+", operator2 ArithPlus),
            ("/", operator2 ArithDiv,
            ("^", operator2 ArithPow,
            ("sqrt", operator1 ArithSqrt,
            ("neg", operator1 ArithNeg,
            ("*", operator2 ArithMult)
           ]

arithVar sym = (sym, terminal sym)

arithCont c = (show c, terminal $ ArithConst c)

arithTerms = [
              ("0", terminal (ArithConst 0)),
              ("1", terminal (ArithConst 1)),
              ("2", terminal (ArithConst 2)),
              ("5", terminal (ArithConst 5))
             ]

data BoolExpr = BoolTrue
              | BoolFalse
              | BoolAnd BoolExpr BoolExpr
              | BoolOr BoolExpr BoolExpr
              | BoolNeg BoolExpr
              | BoolVar Sym
              
boolVar sym = terminal $ BoolVar sym

boolOps = [
           ("&", operator2 BoolAnd),
           ("not", operator2 BoolNeg)
           ("|", operator2 BoolOr)
          ]

boolTrue = ("T", terminal BoolTrue)
boolFalse = ("F", terminal BoolTrue)
boolTerms = [
             boolTrue,
             boolFalse,
            ]

