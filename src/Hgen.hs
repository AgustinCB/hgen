module Hgen ( Params, Evolution ) where

import Population

-- Params Iterations SizePopulation xProbability mProbability
data Params = Params Int Int Double Double

class Evolution a where
  -- user defined
  cross :: [a] -> IO a
  mutate :: a -> IO a
  -- default
  initialization :: Int -> IO (Population a)
  crossover :: Double -> Population a -> IO (Population a)
  mutation :: Double -> Population a -> IO (Population a)
  selection :: Int -> Population a -> IO (Population a)
  geneticAlg :: Params -> IO (Population a)

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
