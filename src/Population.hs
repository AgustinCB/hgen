module Population () where

import Data.Array

class (Array Integer a) => Population a where
  fitness :: Num b => Population a -> Integer -> b
  addPopulation :: Population a -> (Array Integer a) -> Population a
  individual :: Population a -> Integer -> a
  allFitness :: Population a -> [b]
  totalFitness :: Population a -> b
  best :: Population a -> b
  average :: Population a -> Double
  size :: Population a -> Integer
  forAll :: (a -> IO a) -> Population a -> IO (Population a)

  addPopulation pop newPop = pop ++ newPop
  individual pop i = pop ! i
  allFitness pop = map (fitness pop) [0..(size pop) -1]
  totalFitness pop = sum $ allFitness pop
  best pop = maximum $ allFitness pop
  average pop = (totalFitness pop) / (size pop)
  size = length pop
  forAll fn pop = do
    newPop <- map fn pop
    return newPop 
