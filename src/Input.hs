module Input where

import           Limit
import           Utils
import           World

import           Control.Lens
import           Graphics.Gloss.Interface.IO.Game

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton button) Down m p) = do
  view limits >-> fmap _lineStore >-> length >-> show >-> putStrLn
  pointFunc m >-> show >-> putStrLn
  pointFunc m >>= operation button >-> return
 where
  operation LeftButton  = addGridPoint
  operation RightButton = removeGridPoint
  pointFunc Modifiers { shift = Down } = view snapPoint
  pointFunc _                          = const p
handleInputs (EventKey (SpecialKey KeyShiftL) s _ _) =
  s == Down -< set snapState >-> return
handleInputs (EventMotion p) = set mousePos p >-> return
handleInputs _               = return
