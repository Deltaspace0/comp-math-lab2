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
            , dropdown currentEquation [0..length equations-1] f f
            , separatorLine
            , label $ "a = " <> showt a
            , hslider_ pointA (-10) 10 [dragRate 0.1]
            , label $ "b = " <> showt b
            , hslider_ pointB (-10) 10 [dragRate 0.1]
            , label $ "x = " <> showt (model ^. pointRoot)
            , hslider_ pointRoot a b [dragRate 0.05]
            , separatorLine
            , button "Compute next values" AppCompute
            ]
        ] `styleBasic` [padding 16]
    a = model ^. pointA
    b = model ^. pointB
    plot = graph_ (getPoints model)
        [ lockX_ $ model ^. xLock
        , lockY_ $ model ^. yLock
        ]
    methods = [Chord, Newton, Bisection, Iteration, IterationSystem]
    methodTitle x = label $ case x of
        Chord -> "Chord method"
        Newton -> "Newton method"
        Bisection -> "Bisection method"
        Iteration -> "Iteration method"
        IterationSystem -> "Iteration method for systems"
    f x = label $ snd $ equations!!x
