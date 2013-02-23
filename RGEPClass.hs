module RGEPClass where

import RGEP


extractClasses = undefined
testClass clas testCases = undefined
correctlyClassified classifiers testCases = undefines

classifyRGEP :: [([Double], Sym)] -> RGEPParams -> R [(L ArithExpr, Sym)]
classifyRGEP testCases rgepParams = let empty = boolFalse do
  let classes = extractClasses testCases
  let binaryClassifier clas = (fst . snd) <$> simpleRGEPFunction terms ops empty (testClass testCases clas) rgepParams
  classifiers <- mapM binaryClassifier classes
  writeLog $ "best ind: " ++ show elite
  writeLog $ "with fitness: " ++ show fit
  writeLog $ show $ correctlyClassified classifiers
  return classifiers

