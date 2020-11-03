module Layouts
( tall
, monocle
) where

import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier(ModifiedLayout)
import XMonad.Layout.Renamed(renamed, Rename(Replace))

defaultSpacing = 4
goldenRatio = 2/(1+(toRational(sqrt(5)::Double)))

replaceLayout name = renamed [Replace name]

applySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
applySpacing s = spacingRaw False equalSpacedBorder True equalSpacedBorder True
    where equalSpacedBorder = Border s s s s

tall = replaceLayout "tall"
    $ applySpacing defaultSpacing
    $ Tall numOfMasterWindows resizeIncrement ratio
        where numOfMasterWindows = 1
              resizeIncrement = 3/100
              ratio = goldenRatio

monocle = noBorders layout
    where layout = replaceLayout "monocle"
                   $ Full
