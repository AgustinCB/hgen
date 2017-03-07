module Population (Population, Fitness, Chromosome) where

type Fitness a = a -> Double
type Cross a = [a] -> IO a
type Mutate a = a -> IO a

data Chromosome a = Chromosome { cross :: Cross a
                               , mutate :: Mutate a
                               , fitness :: Fitness a }
data Population a = Population [a] (Chromosome a)

addPopulation :: Population a -> [a] -> Population a
addPopulation (Population pop chromosome) newPop = Population (pop ++ newPop) chromosome

individual :: Population a -> Int -> a
individual (Population pop _) i = pop!!i

allFitness :: Population a -> [Double]
allFitness (Population pop (Chromosome _ _ fitness)) = map fitness pop

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
