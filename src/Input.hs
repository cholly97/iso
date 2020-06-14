module Input where

import           Limit
import           Structs.SplayMap
import           Utils.Combinators
import           Utils.FuzzyFloat
import           World

import           Control.Lens
import           Graphics.Gloss.Interface.IO.Game

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton button) Down m p) =
  pointFunc m >>= handleClick
 where
  handleClick :: Point -> World -> IO World
  handleClick = printDebug >>>> operation button >>--> return
  printDebug :: Point -> World -> IO ()
  printDebug p = foldl1 (>>>) $ putStrLn <--<< debugMessages
   where
    debugMessages :: [World -> String]
    debugMessages =
      [ view limits >>--> _lineStore >>--> length . elems >-> show
      , const p >-> show
      , lookupResults >-> show
      ]
    lookupResults :: World -> [Maybe Float]
    lookupResults w =
      let ls = view limits w
          lookupLim l = if find -< FuzzyFloat f -< _lineStore l
            then Just f
            else Nothing
              where f = fst $ pointToLineRep p l
      in  ls >-> lookupLim

  operation LeftButton  = addGridPoint
  operation RightButton = removeGridPoint
  operation _           = const id
  pointFunc Modifiers { shift = Down } = view snapPoint
  pointFunc _                          = const p
handleInputs (EventKey (SpecialKey KeyShiftL) s _ _) =
  s == Down >- set snapState >-> return
handleInputs (EventMotion p) = set mousePos p >-> return
handleInputs _               = return
