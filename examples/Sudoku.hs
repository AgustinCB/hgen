module Main where

import Hgen
import Data.List
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

-- TODO: Check for duplicates
pick2 :: Eq a => [a] -> IO [a]
pick2 xs = do
  one <- pick xs
  two <- pick (filter (/= one) xs)
  return [one, two]

replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

replace2D :: (a -> a) -> Int -> Int -> [[a]] -> [[a]]
replace2D f x y = replace (replace f y) x

type Row = [Int]
type Solution = [Row]
boxSize :: Int
boxSize = 3

intAt :: Solution -> (Int, Int) -> Int
intAt solution (x, y) = (solution !! x) !! y

boxAt :: Solution -> Int -> Row
boxAt solution i = map (intAt solution) [(a + x, b + y) | x <- [0..boxSize-1], y <- [0..boxSize-1]]
  where a = (quot i 3) * 3
        b = (mod i 3) * 3

transposeByBox :: Solution -> Solution
transposeByBox solution = map (boxAt solution) [0..(length solution)-1]

fitnessRow :: Row -> Int
fitnessRow row = (length.nub) row

fitnessSudoku :: Fitness Solution
fitnessSudoku solution = fromIntegral $ sum $ map fitnessRow solution

crossByColumn :: Cross Solution
crossByColumn solutions = do
  sol <- crossByRow $ map transpose solutions
  return $ transpose sol

crossByRow :: Cross Solution
crossByRow solutions = do
  mapM bestRow $ zip (head solutions) (last solutions)
    where bestRow pairRows = do
            let fstRow = fst pairRows
            let sndRow = snd pairRows
            if (fitnessRow fstRow) > (fitnessRow sndRow) then return fstRow
            else return sndRow

crossByBox :: Cross Solution
crossByBox solutions = do
  sol <- crossByRow $ map transposeByBox solutions
  return $ transposeByBox sol

crossSudoku :: Cross Solution
crossSudoku solutions = do
  crossMethod <- pick [crossByColumn, crossByRow, crossByBox]
  crossMethod solutions

mutateSudoku :: Mutate Solution
mutateSudoku solution = do
  newVal <- pick [1..l]
  pos <- pick [(x, y) | x <- [0..l-1], y <- [0..l-1]]
  return $ replace2D (const newVal) (fst pos) (snd pos) solution
    where l = (length solution)

randomSudoku :: RandomInd Solution
randomSudoku _ = do
  mapM randomRow [1..l]
    where l = boxSize * boxSize
          possibilities = [1..l]
          randomRow _ = mapM (\i -> pick possibilities) [1..l]

matingPoolSudoku :: MatingPool Solution
matingPoolSudoku pop = do
  mapM (\_ -> (pick2 matingPool)) [1..(size pop)]
    where solutionProb (solution, fit) = map (\_ -> solution) [1..fit]
          matingPool = concat (map solutionProb (fitnessPairs pop))

sudokuChromosome :: Chromosome Solution
sudokuChromosome = Chromosome crossSudoku mutateSudoku fitnessSudoku randomSudoku matingPoolSudoku

sudokuParams :: Params
sudokuParams = Params 200 50 0.1

printPop :: Population Solution -> IO ()
printPop pop = do
  print "Pooooop!"

main :: IO ()
main = do
  print "Starting!!!"
  pop <- geneticAlg sudokuParams (Population [] sudokuChromosome)
  printPop pop
  print "Ending!!!"
