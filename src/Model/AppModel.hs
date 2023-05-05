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
    , initModel
    , getPoints
    , equations
    ) where

import Control.Lens
import Data.Text (Text)

import Model.Method

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    , _amCalcMethod :: Method
    , _amCurrentEquation :: Int
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    , _amCalcMethod = Chord
    , _amCurrentEquation = 0
    }

getPoints :: AppModel -> [[(Double, Double)]]
getPoints model
    | e == 0 =
        [ f <$> [-10, -9.98..10]
        , [(-4, 0)]
        , [(-3, 0)]
        , [(-2, 0)]
        , [(-1, 0)]
        , [(1, 0)]
        , [(2, 0)]
        ]
    | otherwise = [f <$> [-10, -9.98..10]]
    where
        e = model ^. currentEquation
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
