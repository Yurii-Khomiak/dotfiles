module Layouts
( tallEqual
, tallGolden
, monocle
) where

import XMonad.Layout(Tall(..), Full(..))
import XMonad.Layout.Spacing(spacingRaw, Spacing, Border(..))
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.LayoutModifier(ModifiedLayout)
import XMonad.Layout.Renamed(renamed, Rename(Replace))

defaultSpacing = 4
goldenRatio = 2/(1+(toRational(sqrt(5)::Double)))

replaceLayout name = renamed [Replace name]

applySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
applySpacing s = spacingRaw False equalSpacedBorder True equalSpacedBorder True
    where equalSpacedBorder = Border s s s s

tall ratio = replaceLayout "tall"
    $ applySpacing defaultSpacing
    $ Tall numOfMasterWindows resizeIncrement ratio
        where numOfMasterWindows = 1
              resizeIncrement = 3/100

tallEqual = tall (1/2)
tallGolden = tall goldenRatio

monocle = noBorders layout
    where layout = replaceLayout "monocle"
                   $ Full

