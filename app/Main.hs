module Main where

import           Lib
import           Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO display
              bgColor
              fps
              initWorld
              displayWorld
              handleInputs
              timeUpdate
 where
  display = FullScreen
  fps     = 144
  bgColor = white
