module Neural where

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
annGA neuron trainingSet@(inputs, outputs) arch gaparams = simpleGA fitness gaparams randomIndividual where
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

