module Lib
    ( someFunc
    ) where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

fps = 60
bgColor = white
type World = Float

initWorld :: World
initWorld = 42.0

displayWorld :: World -> IO Picture
displayWorld w = return (Circle w)

handleInputs :: Event -> World -> IO World
handleInputs _ _ = return initWorld

timeUpdate :: Float -> World -> IO World
timeUpdate dt w = return (w + dt)

window :: Display
window = FullScreen

someFunc :: IO ()
someFunc = playIO window bgColor fps initWorld displayWorld handleInputs timeUpdate
