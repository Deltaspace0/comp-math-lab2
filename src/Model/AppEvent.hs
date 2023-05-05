module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Monomer

import Model.AppModel

data AppEvent
    = AppInit
    | AppCompute
    | AppSetIterations Int
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppCompute -> computeHandle model
    AppSetIterations v -> setIterationsHandle v model

computeHandle :: EventHandle
computeHandle model = response where
    response =
        [ Model $ model
            & pointA .~ a'
            & pointB .~ b'
            & pointRoot .~ x'
            & iterations +~ 1
        ]
    (a', b', x') = compute (model ^. calcMethod) f d a b x
    f = snd . (fst $ equations!!(model ^. currentEquation))
    d = derivatives!!(model ^. currentEquation)
    a = model ^. pointA
    b = model ^. pointB
    x = model ^. pointRoot

setIterationsHandle :: Int -> EventHandle
setIterationsHandle v model = [Model $ model & iterations .~ v]
