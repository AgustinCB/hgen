module Main where

import Hgen
import Data.List
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

type Solution = [[Int]]

fitnessRow :: [Int] -> Int
fitnessRow row = (length.nub) row

fitness :: Fitness Solution
fitness solution = fromIntegral $ sum $ map fitnessRow solution

crossByColumn :: Cross Solution
crossByColumn solutions = do
  sol <- crossByRow $ map transpose solutions
  return transpose sol

crossByRow :: Cross Solution
crossByRow solutions = do
  mapM bestRow $ zip (head solutions) (last solutions)
    where bestRow pairRows = do
            let fstRow = fst pairRows
            let sndRow = snd pairRows
            if (fitnessRow fstRow) > (fitnessRow sndRow) then return fstRow
            else return sendRow

crossBySubgroup :: Cross Solution

cross :: Cross Solution
cross solutions = do
  crossMethod <- pick [crossByColumn, crossByRow, crossBySubgroup]
  crossMethod solutions

main :: IO ()
main = do
  print "Hello world"
