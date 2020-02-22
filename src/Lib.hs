{-# LANGUAGE TemplateHaskell #-}
module Lib
  ( someFunc
  )
where

import           Control.Lens
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

fps = 60

bgColor = white

data LimitPoint
  = Point Point
  | Angle Float

type Triple a = (a, a, a)

data Grid = Grid
  { _spacing :: Float
  , _limitPoints :: Triple LimitPoint
  }

data World = World
  { _grid :: Grid
  }

$(makeLenses ''World)
$(makeLenses ''Grid)

initWorld :: World
initWorld = World
  { _grid = Grid { _spacing     = 42.0
                 , _limitPoints = (Angle 1.0, Angle 2.0, Point (3.0, 4.0))
                 }
  }

displayWorld :: World -> IO Picture
displayWorld = return . Circle . _spacing . _grid

handleInputs :: Event -> World -> IO World
handleInputs _ _ = return initWorld

timeUpdate :: Float -> World -> IO World
timeUpdate = (return .) . (grid . spacing %~) . (+) . (* 10)
-- timeUpdate dt w = return ((over (grid . spacing) (+ (dt * 10))) w)

window :: Display
window = FullScreen

someFunc :: IO ()
someFunc =
  playIO window bgColor fps initWorld displayWorld handleInputs timeUpdate
