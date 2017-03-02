module Hgen
    ( Chromosome, K, Input, Evaluation, Adaptation, Reproduction, Mutation
    ) where

type K = Int
type Input = [ Int ]
type Evaluation = Input -> Int
type Adaptation = Int -> K -> Int
type Reproduction = Input -> Input -> (Input, Input)
type Mutation = Input -> Input
data Chromosome = Chromosome Evaluation Adaptation Reproduction Mutation

data Evolution = Evolution Input Chromosome Int Int

--start :: Evolution -> [ Input ] -> IO Input
--start (Evolution evaluation adaptation reproduction mutation) inputs = 
