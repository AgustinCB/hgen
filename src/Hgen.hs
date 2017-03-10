module Hgen ( Params, Evolution, Population, Chromosome, Fitness, Cross, Mutate, RandomInd, MatingPool ) where

import Data.List (sortBy)
import Data.Ord

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

instance Evolution (Population a) where
  initialization size p = randomPopulation size p
  crossover p = crossPopulation p
  mutation prob p = mutatePopulation prob p
  selection size p = do return (limit (sort p) size)
  geneticAlg (Params iterations sizePop mPro) population = do
    pop <- initialization sizePop population
    doit iterations pop
      where doit n pop =
              if n == 0 then return pop
              else
                crossover pop >>=
                mutation mPro >>=
                selection sizePop >>=
                doit (n-1)
