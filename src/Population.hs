module Population () where

import Data.Array

type Fitness a = Population a -> Integer -> Double
data Population a = Population [a] (Fitness a)

addPopulation :: Population a -> [a] -> Population a
addPopulation (Population pop fitness) newPop = Population (pop ++ newPop) fitness

individual :: Population a -> Integer -> a
individual (Population pop _) i = pop ! i

allFitness :: Population a -> [b]
allFitness (Population pop fitness) = map (fitness pop) [0..(size pop) -1]

totalFitness :: Population a -> b
totalFitness pop = sum $ allFitness pop

best :: Population a -> b
best pop = maximum $ allFitness pop

average :: Population a -> Double
average pop = (totalFitness pop) / (size pop)

size :: Population a -> Integer
size = length pop

forAll :: (a -> IO a) -> Population a -> IO (Population a)
forAll fn pop = do
  newPop <- map fn pop
  return newPop 
