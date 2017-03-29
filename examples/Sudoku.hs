module Main where

import Hgen
import Data.List
import Data.Maybe
import System.Random (randomRIO)

pick :: [a] -> Maybe (IO a)
pick [] = Nothing
pick xs = Just (fmap (xs !!) $ randomRIO (0, length xs - 1))

pickVal :: [a] -> IO a
pickVal xs = (fromJust $ pick xs)

-- TODO: Check for duplicates
pick2 :: Eq a => [a] -> IO [a]
pick2 xs =
  let first = pick xs
      oneIO = (fromJust first)
  in do
    one <- oneIO
    two <- fromMaybe oneIO (pick (filter (/= one) xs))
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
fitnessRow row = ((length.nub) row) - 1

fitnessSudoku :: Fitness Solution
fitnessSudoku solution =
                    (fromIntegral $ sum $ map fitnessRow solution) +
                    (fromIntegral $ sum $ map fitnessRow (transpose solution)) +
                    (fromIntegral $ sum $ map fitnessRow (transposeByBox solution))

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
  crossMethod <- pickVal [crossByRow, crossByBox, crossByColumn]
  crossMethod solutions

mutateSudoku :: Mutate Solution
mutateSudoku solution = do
  newVal <- pickVal [1..l]
  pos <- pickVal [(x, y) | x <- [0..l-1], y <- [0..l-1]]
  return $ replace2D (const newVal) (fst pos) (snd pos) solution
    where l = (length solution)

randomSudoku :: RandomInd Solution
randomSudoku _ = do
  mapM randomRow [1..l]
    where l = boxSize * boxSize
          possibilities = [1..l]
          randomRow _ = mapM (\i -> pickVal possibilities) [1..l]

matingPoolSudoku :: MatingPool Solution
matingPoolSudoku pop = do
  mapM (\_ -> (pick2 matingPool)) [1..(size pop)]
    where solutionProb (solution, fit) = map (\_ -> solution) [1..fit]
          matingPool = concat (map solutionProb (fitnessPairs pop))

showChromosome :: ShowInd Solution
showChromosome solution = (show solution)

sudokuChromosome :: Chromosome Solution
sudokuChromosome = Chromosome crossSudoku mutateSudoku fitnessSudoku randomSudoku matingPoolSudoku showChromosome

sudokuParams :: Params
sudokuParams = Params 500 50 0.1

printPop :: Population Solution -> IO ()
printPop (Population pop c) = do
  printSol pop c
  where printSol [] c = do print ""
        printSol pop c = do
          print (show (head pop))
          printSol (tail pop) c

main :: IO ()
main = do
  print "Starting!!!"
  pop <- geneticAlg sudokuParams (Population [] sudokuChromosome)
  print "And the winer is:"
  print (show (head (population pop)))
  print (show (fitnessSudoku (head (population pop))))
  print "Ending!!!"
