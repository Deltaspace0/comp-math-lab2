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
    response = [Model newModel]
    newModel = if model ^. calcMethod == IterationSystem
        then model
            & pointRoot1 .~ x1'
            & pointRoot2 .~ x2'
            & previousRoot1 .~ Just x1
            & previousRoot2 .~ Just x2
            & iterations +~ 1
        else model
            & pointA .~ a'
            & pointB .~ b'
            & pointRoot .~ x'
            & previousRoot .~ Just x
            & iterations +~ 1
    (x1', x2') = computeSystem f1 f2 x1 x2
    f1 = systemF1!!(model ^. systemEquation1)
    f2 = systemF2!!(model ^. systemEquation2)
    x1 = model ^. pointRoot1
    x2 = model ^. pointRoot2
    (a', b', x') = compute (model ^. calcMethod) f d a b x
    f = snd . (fst $ equations!!(model ^. currentEquation))
    d = derivatives!!(model ^. currentEquation)
    a = model ^. pointA
    b = model ^. pointB
    x = model ^. pointRoot

setIterationsHandle :: Int -> EventHandle
setIterationsHandle v model = [Model $ model & iterations .~ v]
