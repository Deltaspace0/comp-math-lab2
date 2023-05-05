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
    , systemEquation1
    , systemEquation2
    , pointA
    , pointB
    , pointRoot
    , pointRoot1
    , pointRoot2
    , previousRoot
    , previousRoot1
    , previousRoot2
    , iterations
    , initModel
    , getPoints
    , getDx
    , getDx1
    , getDx2
    , equations
    , derivatives
    , systems1
    , systems2
    , systemF1
    , systemF2
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import TextShow

import Model.Method

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    , _amCalcMethod :: Method
    , _amCurrentEquation :: Int
    , _amSystemEquation1 :: Int
    , _amSystemEquation2 :: Int
    , _amPointA :: Double
    , _amPointB :: Double
    , _amPointRoot :: Double
    , _amPointRoot1 :: Double
    , _amPointRoot2 :: Double
    , _amPreviousRoot :: Maybe Double
    , _amPreviousRoot1 :: Maybe Double
    , _amPreviousRoot2 :: Maybe Double
    , _amIterations :: Int
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    , _amCalcMethod = Chord
    , _amCurrentEquation = 0
    , _amSystemEquation1 = 0
    , _amSystemEquation2 = 0
    , _amPointA = -1
    , _amPointB = 1
    , _amPointRoot = 0
    , _amPointRoot1 = 0
    , _amPointRoot2 = 0
    , _amPreviousRoot = Nothing
    , _amPreviousRoot1 = Nothing
    , _amPreviousRoot2 = Nothing
    , _amIterations = 0
    }

getPoints :: AppModel -> [[(Double, Double)]]
getPoints model = points where
    points = if model ^. calcMethod == IterationSystem
        then
            [ f1 <$> [-10, -9.98..10]
            , f2 <$> [-10, -9.98..10]
            , [(model ^. pointRoot1, model ^. pointRoot2)]
            ]
        else
            [ f <$> [-10, -9.98..10]
            , case model ^. calcMethod of
                Chord -> [f a, f b]
                Newton -> [nf a, nf b]
                _ -> []
            , [(a, 0)]
            , [(b, 0)]
            , [(x, 0)]
            ]
    nf t = (t, (snd $ f x) - (d x)*(x-t))
    d = derivatives!!i
    f = fst $ equations!!i
    f1 = fst $ systems1!!(model ^. systemEquation1)
    f2 = fst $ systems2!!(model ^. systemEquation2)
    i = model ^. currentEquation
    a = model ^. pointA
    b = model ^. pointB
    x = model ^. pointRoot

getDx :: AppModel -> Text
getDx model = let px = model ^. previousRoot in if null px
    then "none"
    else showt $ (model ^. pointRoot) - (fromJust px)

getDx1 :: AppModel -> Text
getDx1 model = let px = model ^. previousRoot1 in if null px
    then "none"
    else showt $ (model ^. pointRoot1) - (fromJust px)

getDx2 :: AppModel -> Text
getDx2 model = let px = model ^. previousRoot2 in if null px
    then "none"
    else showt $ (model ^. pointRoot2) - (fromJust px)

equations :: [((Double -> (Double, Double)), Text)]
equations =
    [
        ( (\x -> (x, -1.38*x**3-5.42*x**2+2.57*x+10.95))
        , "-1.38x^3 - 5.42x^2 + 2.57x + 10.95 = 0"
        )
    ,   ( (\x -> (x, x-cos x))
        , "x - cos x = 0"
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

systems1 :: [((Double -> (Double, Double)), Text)]
systems1 =
    [ 
        (\t -> (cos t, sin t)
        , "x^2 + y^2 = 1"
        )
    ]

systems2 :: [((Double -> (Double, Double)), Text)]
systems2 =
    [
        (\t -> (t**2-1, t)
        , "x - y^2 + 1 = 0"
        )
    ]

systemF1 :: [(Double -> Double -> Double)]
systemF1 =
    [ \x y -> x**2+y**2-1
    ]

systemF2 :: [Double -> Double -> Double]
systemF2 =
    [ \x y -> x-y**2+1
    ]
