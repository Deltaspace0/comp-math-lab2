{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.Method
    , AppModel(..)
    , xLock
    , yLock
    , calcMethod
    , currentEquation
    , pointA
    , pointB
    , pointRoot
    , initModel
    , getPoints
    , equations
    , derivatives
    ) where

import Control.Lens
import Data.Text (Text)

import Model.Method

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    , _amCalcMethod :: Method
    , _amCurrentEquation :: Int
    , _amPointA :: Double
    , _amPointB :: Double
    , _amPointRoot :: Double
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    , _amCalcMethod = Chord
    , _amCurrentEquation = 0
    , _amPointA = -1
    , _amPointB = 1
    , _amPointRoot = 0
    }

getPoints :: AppModel -> [[(Double, Double)]]
getPoints model = points where
    points =
        [ f <$> [-10, -9.98..10]
        , [(model ^. pointA, 0)]
        , [(model ^. pointB, 0)]
        , [(model ^. pointRoot, 0)]
        ]
    f = fst $ equations!!(model ^. currentEquation)

equations :: [((Double -> (Double, Double)), Text)]
equations =
    [
        ( (\x -> (x, -1.38*x**3-5.42*x**2+2.57*x+10.95))
        , "-1.38x^3 - 5.42x^2 + 2.57x + 10.95 = 0"
        )
    ,   ( (\x -> (x, x-cos x))
        , "x-cos x = 0"
        )
    ,   ( (\x -> (x, x**2 - 2))
        , "x^2 - 2 = 0"
        )
    ,   ( (\x -> (x, (exp x)-x**2))
        , "e^x - x^2 = 0"
        )
    ]

derivatives :: [Double -> Double]
derivatives =
    [ \x -> (-1.38*3)*x**2-(5.42*2)*x+2.57
    , \x -> 1+sin x
    , \x -> 2*x
    , \x -> (exp x)-2*x
    ]
