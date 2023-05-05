module Model.Method
    ( Method(..)
    , compute
    , computeSystem
    ) where

data Method
    = Chord
    | Newton
    | Bisection
    | Iteration
    | IterationSystem
    deriving (Eq, Show)

compute
    :: Method
    -> (Double -> Double)
    -> (Double -> Double)
    -> Double
    -> Double
    -> Double
    -> (Double, Double, Double)
compute Chord f _ a b _ = (a', b', x') where
    (a', b') = if fa*(f x') <= 0
        then (a, x')
        else (x', b)
    x' = (a*fb-b*fa)/(fb-fa)
    (fa, fb) = (f a, f b)
compute Newton f d a b x = (a, b, x') where
    x' = x - (f x)/(d x)
compute Bisection f _ a b _ = (a', b', x') where
    (a', b') = if (f a)*(f x') <= 0
        then (a, x')
        else (x', b)
    x' = (a+b)/2
compute Iteration f d a b x = (a, b, x') where
    x' = (l*(f x) + x)
    l = -1/(max (d a) (d b))
compute _ _ _ a b x = (a, b, x)

computeSystem
    :: (Double -> Double -> Double)
    -> (Double -> Double -> Double)
    -> Double
    -> Double
    -> (Double, Double)
computeSystem f1 f2 x1 x2 = (x1', x2') where
    x1' = x1-0.1*(f1 x1 x2)
    x2' = x2-0.1*(f2 x1 x2)
