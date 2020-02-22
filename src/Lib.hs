module Lib
    ( someFunc
    ) where

import           Graphics.Gloss

someFunc :: IO ()
someFunc = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
