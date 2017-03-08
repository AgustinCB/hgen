module Hgen ( Params, Evolution, Population, Chromosome ) where

import Data.List (sortBy)
import Data.Ord

import Population

-- Params Iterations SizePopulation xProbability mProbability
data Params = Params { iterations :: Int
                     , sizePopulation :: Int
                     , xProbability :: Double
                     , yProbability :: Double }

class Evolution population where
  -- default
  initialization :: Int -> IO population
  crossover :: Double -> population -> IO population
  mutation :: Double -> population -> IO population
  selection :: Int -> population -> IO population
  geneticAlg :: Params -> IO population

instance Evolution (Population a) where
  selection size p = do return (limit (sort p) size)
  geneticAlg (Params iterations sizePop xPro mPro) = do
    pop <- initialization sizePop
    doit iterations pop
      where doit n pop =
              if n == 0 then return pop
              else
                crossover xPro pop >>=
                mutation mPro >>=
                selection sizePop >>=
                doit (n-1)
