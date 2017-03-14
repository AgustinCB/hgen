module Hgen (Params(..), Evolution(..), Population(..), Chromosome(..), Fitness, Cross, Mutate, RandomInd, MatingPool, allFitness, fitnessPairs, size) where

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
  selection sizePop p = do
    print "IN SELECTION"
    print ("SIZE " ++ (show (size p)))
    print ("SIZE " ++ (show (size (sort p))))
    return (limit (sort p) sizePop)
  geneticAlg (Params iterations sizePop mPro) pop = do
    print "Before initialization"
    initPop <- initialization sizePop pop
    print "After initialization"
    print ("iterations " ++ (show iterations))
    doit iterations initPop
      where doit n initPop@(Population p _) = do
              print "iteration"
              print ("size " ++ (show (size initPop)))
              if n == 0 then return initPop
              else
                crossover initPop >>=
                mutation mPro >>=
                selection sizePop >>=
                doit (n-1)
