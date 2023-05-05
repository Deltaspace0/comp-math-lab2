module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Graph

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box $ plot `styleBasic` [sizeReqW $ fixedSize 600]
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ labeledCheckbox "Lock X" xLock
            , labeledCheckbox "Lock Y" yLock
            ]
        ] `styleBasic` [padding 16]
    plot = graph_ (getPoints model)
        [ lockX_ $ model ^. xLock
        , lockY_ $ model ^. yLock
        ]
