module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Graph
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box $ plot `styleBasic` [sizeReqW $ fixedSize 400]
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ labeledCheckbox "Lock X" xLock
            , labeledCheckbox "Lock Y" yLock
            , separatorLine
            , dropdown calcMethod methods methodTitle methodTitle
            , dropdown currentEquation [0..length equations-1] et et
            , separatorLine
            , label $ "a = " <> showt a
            , hslider_ pointA (-10) 10 [dragRate 0.1]
            , label $ "b = " <> showt b
            , hslider_ pointB (-10) 10 [dragRate 0.1]
            , label $ "x = " <> showt x
            , hslider_ pointRoot a b [dragRate 0.05]
            , label $ "f(x) = " <> showt (f x)
            , label $ "Iterations: " <> showt (model ^. iterations)
            , separatorLine
            , button "Compute next values" AppCompute
            , button "Reset iterations" $ AppSetIterations 0
            ]
        ] `styleBasic` [padding 16]
    a = model ^. pointA
    b = model ^. pointB
    x = model ^. pointRoot
    f = snd . (fst $ equations!!(model ^. currentEquation))
    plot = graph_ (getPoints model)
        [ lockX_ $ model ^. xLock
        , lockY_ $ model ^. yLock
        ]
    methods = [Chord, Newton, Bisection, Iteration, IterationSystem]
    methodTitle t = label $ case t of
        Chord -> "Chord method"
        Newton -> "Newton method"
        Bisection -> "Bisection method"
        Iteration -> "Iteration method"
        IterationSystem -> "Iteration method for systems"
    et t = label $ snd $ equations!!t
