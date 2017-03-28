module Population (Population(..), Fitness, Cross, Mutate, RandomInd, MatingPool, ShowInd, Chromosome(..), limit, sort, mutatePopulation, crossPopulation, randomPopulation, allFitness, fitnessPairs, size, showPopulation) where

import Data.List (sortBy, intercalate)
import Data.Ord
import Debug.Trace
import System.Random

type Fitness a = a -> Double
type Cross a = [a] -> IO a
type Mutate a = a -> IO a
type RandomInd a = Int -> IO a
type MatingPool a = Population a -> IO [[a]]
type ShowInd a = a -> String

data Chromosome a = Chromosome { cross :: Cross a, mutate :: Mutate a, fitness :: Fitness a, randomInd :: RandomInd a, matingPool :: MatingPool a, showInd :: ShowInd a }
data Population a = Population { population :: [a], chromosome :: (Chromosome a) }

addPopulation :: Population a -> [a] -> Population a
addPopulation (Population pop chromosome) newPop = Population (pop ++ newPop) chromosome

individual :: Population a -> Int -> a
individual (Population pop _) i = pop!!i

showPopulation :: Population a -> IO()
showPopulation (Population pop c) = do
  printPop pop c
  where printPop [] c = do print ""
        printPop pop c = do
          print ((showInd c) (head pop))
          printPop (tail pop) c

allFitness :: Population a -> [Double]
allFitness (Population pop chromosome) = map (fitness chromosome) pop

fitnessPairs :: Population a -> [(a, Double)]
fitnessPairs p@(Population pop _) = zip pop (allFitness p)

totalFitness :: Population a -> Double
totalFitness pop = sum $ allFitness pop

best :: Population a -> Double
best pop = maximum $ allFitness pop

average :: Population a -> Double
average pop = (totalFitness pop) / (fromIntegral (size pop))

size :: Population a -> Int
size (Population pop _) = length pop

randomFn :: Population a -> (a -> IO a) -> Double -> Int -> IO a
randomFn pop fn prob i = do
  res <- randomIO :: IO Double
  if res <= prob then (fn (individual pop i))
  else return (individual pop i)

mutatePopulation :: Double -> Population a -> IO (Population a)
mutatePopulation prob p@(Population _ c) = do
  newPop <- (mapM (randomFn p (mutate c) prob) [0..(size p)-1])
  return (Population newPop c)

crossPopulation :: Population a -> IO (Population a)
crossPopulation p@(Population _ chromosome) = do
  matingPool <- (matingPool chromosome) p
  children <- mapM (cross chromosome) matingPool
  return $ addPopulation p children

randomPopulation :: Int -> Population a -> IO (Population a)
randomPopulation size (Population _ chromosome) = do
  pop <- mapM (randomInd chromosome) [0..size-1]
  return (Population pop chromosome)

limit :: Population a -> Int -> Population a
limit (Population pop c) size = (Population (take size pop) c)

sort :: Population a -> Population a
sort (Population pop c@(Chromosome _ _ fitness _ _ showSolution)) = (Population (sortBy compare pop) c)
  where compare sol1 sol2
          | (fitness sol1) > (fitness sol2) = LT
          | (fitness sol2) > (fitness sol1) = GT
          | otherwise                       = EQ

forAll :: (a -> IO a) -> Population a -> IO (Population a)
forAll fn (Population pop chromosome) = do
  newPop <- mapM fn pop
  return (Population newPop chromosome)
