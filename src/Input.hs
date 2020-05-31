module Input where

import           Limit
import           Utils.Combinators
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
      [ view limits >-> fmap _lineStore >-> fmap length >-> show
      , const p >-> show
      , lookupResults >-> show
      ]
    lookupResults :: World -> [Maybe Float]
    lookupResults =
      fst
        <--<< fuzzyLookup
        .     fst
        .     pointToLineRep p
        <*>   _lineStore
        <--<< view limits
  operation LeftButton  = addGridPoint
  operation RightButton = removeGridPoint
  operation _           = const id
  pointFunc Modifiers { shift = Down } = view snapPoint
  pointFunc _                          = const p
handleInputs (EventKey (SpecialKey KeyShiftL) s _ _) =
  s == Down >- set snapState >-> return
handleInputs (EventMotion p) = set mousePos p >-> return
handleInputs _               = return
