module Model.Method
    ( Method(..)
    ) where

data Method
    = Chord
    | Newton
    | Bisection
    | Iteration
    | IterationSystem
    deriving (Eq, Show)
