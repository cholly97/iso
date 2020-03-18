module Main where

import           Display
import           Input
import           World

import           Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  playIO display bgColor fps initWorld displayWorld handleInputs timeUpdate
 where
  display = FullScreen
  fps     = 144
  bgColor = white
