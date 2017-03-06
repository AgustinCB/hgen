module Population (Population, Fitness) where

type Fitness a = a -> Double
data Population a = Population [a] (Fitness a)

addPopulation :: Population a -> [a] -> Population a
addPopulation (Population pop fitness) newPop = Population (pop ++ newPop) fitness

individual :: Population a -> Int -> a
individual (Population pop _) i = pop!!i

allFitness :: Population a -> [Double]
allFitness (Population pop fitness) = map fitness pop

totalFitness :: Population a -> Double
totalFitness pop = sum $ allFitness pop

best :: Population a -> Double
best pop = maximum $ allFitness pop

average :: Population a -> Double
average pop = (totalFitness pop) / (fromIntegral (size pop))

size :: Population a -> Int
size (Population pop _) = length pop

forAll :: (a -> IO a) -> Population a -> IO (Population a)
forAll fn pop = do
  return pop
