{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , xLock
    , yLock
    , initModel
    , getPoints
    ) where

import Control.Lens

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    }

getPoints :: AppModel -> [[(Double, Double)]]
getPoints _ = points where
    points =
        [ (\x -> (x, -1.38*x**3-5.42*x**2+2.57*x+10.95)) <$> [-10, -9.98..10]
        , [(-4, 0)]
        , [(-3, 0)]
        , [(-2, 0)]
        , [(-1, 0)]
        , [(1, 0)]
        , [(2, 0)]
        ]
