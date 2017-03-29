module Hgen (Params(..), Evolution(..), Population(..), Chromosome(..), Fitness, Cross, Mutate, RandomInd, MatingPool, ShowInd, allFitness, fitnessPairs, size) where

import Data.List (sortBy, intercalate)
import Data.Ord
import Debug.Trace

import Population

-- Params Iterations SizePopulation xProbability mProbability
data Params = Params { iterations :: Int
                     , sizePopulation :: Int
                     , yProbability :: Double }

class Evolution population where
  -- default
  initialization :: Int -> population -> IO population
  crossover :: population -> IO population
  mutation :: Double -> population -> IO population
  selection :: Int -> population -> IO population
  geneticAlg :: Params -> population -> IO population
  getBetter :: population -> population
  showAll :: population -> IO ()

instance Evolution (Population a) where
  initialization size p = randomPopulation size p
  crossover p = crossPopulation p
  mutation prob p = mutatePopulation prob p
  showAll p = showPopulation p
  selection sizePop p@(Population _ c) = do
    return (limit (sort p) sizePop)
  getBetter p@(Population _ c) = (limit (sort p) (fromIntegral 1 :: Int))
  geneticAlg (Params iterations sizePop mPro) pop = do
    initPop <- initialization sizePop pop
    doit iterations initPop
      where doit n initPop@(Population p _) = do
              if n == 0 then return initPop
              else
                crossover initPop >>=
                mutation mPro >>=
                selection sizePop >>=
                doit (n-1)
