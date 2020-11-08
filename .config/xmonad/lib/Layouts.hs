module Layouts
( tall
, tallGolden
, wide
, wideGolden
, monocle
) where

import qualified XMonad.Layout as L
import XMonad.Layout.Fullscreen(fullscreenFull)
import XMonad.Layout.Spacing(spacingRaw, Spacing, Border(..))
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.LayoutModifier(ModifiedLayout)
import XMonad.Layout.Renamed(renamed, Rename(Replace))

defaultSpacing = 4
goldenRatio = 2/(1+(toRational(sqrt(5)::Double)))

rename name = renamed [Replace name]

applySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
applySpacing s = spacingRaw False equalSpacedBorder True equalSpacedBorder True
    where equalSpacedBorder = Border s s s s

makeTall ratio = rename "Tall"
    $ applySpacing defaultSpacing
    $ L.Tall numOfMasterWindows resizeIncrement ratio
        where numOfMasterWindows = 1
              resizeIncrement = 3/100

tallAndWide r = (t, w)
    where t = makeTall r
          w = rename "Wide" $ L.Mirror t

(tall, wide) = tallAndWide (1/2)
(tallGolden, wideGolden) = tallAndWide goldenRatio

monocle = noBorders layout
    where layout = rename "Monocle"
            $ fullscreenFull
            $ L.Full

