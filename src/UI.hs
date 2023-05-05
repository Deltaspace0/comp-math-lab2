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
            [ hgrid
                [ labeledCheckbox "Lock X" xLock
                , labeledCheckbox "Lock Y" yLock
                ]
            , dropdown calcMethod methods methodTitle methodTitle
            , separatorLine
            , if model ^. calcMethod == IterationSystem
                then systemEquationPanel
                else singleEquationPanel
            , separatorLine
            , label $ "Iterations: " <> showt (model ^. iterations)
            , button "Compute next values" AppCompute
            , button "Reset iterations" $ AppSetIterations 0
            ]
        ] `styleBasic` [padding 16]
    singleEquationPanel = vstack_ [childSpacing_ 16]
        [ dropdown currentEquation [0..length equations-1] et et
        , separatorLine
        , label $ "a = " <> showt a
        , hslider_ pointA (-10) 10 [dragRate 0.1]
        , label $ "b = " <> showt b
        , hslider_ pointB (-10) 10 [dragRate 0.1]
        , label $ "x = " <> showt x
        , hslider_ pointRoot a b [dragRate 0.05]
        , label $ "dx = " <> getDx model
        , label $ "f(x) = " <> showt (f x)
        ]
    systemEquationPanel = vstack_ [childSpacing_ 16]
        [ dropdown systemEquation1 [0..length systems1-1] st1 st1
        , dropdown systemEquation2 [0..length systems2-1] st2 st2
        , separatorLine
        , label $ "x = " <> showt x1
        , hslider_ pointRoot1 (-5) 5 [dragRate 0.1]
        , label $ "y = " <> showt x2
        , hslider_ pointRoot2 (-5) 5 [dragRate 0.1]
        , hgrid
            [ label $ "dx = " <> getDx1 model
            , label $ "dy = " <> getDx2 model
            ]
        , label $ "f(x, y) = " <> showt (f1 x1 x2)
        , label $ "g(x, y) = " <> showt (f2 x1 x2)
        ]
    a = model ^. pointA
    b = model ^. pointB
    x = model ^. pointRoot
    x1 = model ^. pointRoot1
    x2 = model ^. pointRoot2
    f1 = systemF1!!(model ^. systemEquation1)
    f2 = systemF2!!(model ^. systemEquation2)
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
    st1 t = label $ snd $ systems1!!t
    st2 t = label $ snd $ systems2!!t
