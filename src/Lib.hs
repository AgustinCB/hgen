module Lib
    ( Chromosome, K, Input
    ) where

type K = Int
type Input = [ Int ]
type Evaluation = Input -> Int
type Adaptation = Int -> K -> Int
type Reproduction = Input -> Input -> (Input, Input)
type Mutation = Input -> Input
data Chromosome = Chromosome Evaluation Adaptation Reproduction Mutation


