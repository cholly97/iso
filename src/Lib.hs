{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Data.Maybe
import           Control.Lens
import           Control.Monad
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Gloss.Interface.Environment
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt

type Bounds = (Float, Float)

data Limit
  = LimitPoint Point
  | Angle Float

type Triple a = (a, a, a)

data Grid = Grid
  { _spacing :: Float
  , _limits :: [Limit]
  }

data World = World
  , _origins :: [Point]
  { _bounds :: Maybe Bounds
  , _grid :: Grid
  }

$(makeLenses ''World)
$(makeLenses ''Grid)

origin :: Point
origin = (0, 0)

initWorld :: World
initWorld = World
  { _bounds  = origin
  , _origins = [origin]
  , _grid    = Grid
                 { _spacing = 42.0
                 , _limits  = [ Angle (pi / 2)
                              , LimitPoint (rotateV (-pi * 1 / 6) (400, 0))
                              -- , Angle (-pi * 1 / 6)
                              , LimitPoint (rotateV (-pi * 5 / 6) (400, 0))
                              -- , Angle (-pi * 5 / 6)
                              ]
                 }
  }

displayWorld :: World -> IO Picture
displayWorld = return . Pictures . ap ((:) . Circle . _spacing . _grid)
                                      (([drawGrid] <*>) . pure)

drawGrid :: World -> Picture
drawGrid world =
  Pictures
    $   [drawLimitPoints]
    ++  ([drawGridLines . _bounds $ world] <*> _origins world)
    <*> (world ^. grid . limits)

drawLimitPoints :: Limit -> Picture
drawLimitPoints (Angle      _) = Blank
drawLimitPoints (LimitPoint p) = translateP p $ Circle 10

drawGridLines :: Bounds -> Point -> Limit -> Picture
drawGridLines bounds o (Angle      a) = linePA bounds o a
drawGridLines bounds o (LimitPoint p) = linePA bounds o . argV $ p Pt.- o

translateP :: Point -> Picture -> Picture
translateP = uncurry Translate

rayPA :: Bounds -> Point -> Float -> Picture
rayPA (w, h) p a =
  translateP p . (Rotate (radToDeg (-a))) $ Line [origin, (w + h, 0)]

linePA :: Bounds -> Point -> Float -> Picture
linePA (w, h) p a =
  translateP p . (Rotate (radToDeg (-a))) $ Line [(-w - h, 0), (w + h, 0)]

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton LeftButton) Down undefined p) =
  return . over origins (p :)
handleInputs _ = return

timeUpdate :: Float -> World -> IO World
timeUpdate dt world = do
  let world' = (grid . spacing %~ (+ (dt * 0))) world
  if _bounds world == Nothing
    then do
      setBounds world'
    else do
      return world'

setBounds :: World -> IO World
setBounds world = do
  gs <- getScreenSize
  let s = Just . join bimap fromIntegral $ gs
  return . set bounds s $ world
