{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Data.Maybe
import qualified Data.Set                      as Set
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

type Angle = Float

data Limit
  = Infinite {angle :: Angle, normal :: Vector, dists :: Set.Set Float}
  | Finite {point :: Point, angles :: Set.Set Angle}

type Triple a = (a, a, a)

data Grid = Grid
  { _spacing :: Float
  , _limits :: [Limit]
  }

data World = World
  { _bounds :: Maybe Bounds
  , _grid :: Grid
  }

$(makeLenses ''World)
$(makeLenses ''Grid)

origin :: Point
origin = (0, 0)

initWorld :: World
initWorld = World
  { _bounds = Nothing
  , _grid   = Grid
                { _spacing = 42.0
                , _limits  = [ Infinite (pi / 2) (1, 0) Set.empty
                             , Finite (rotateV (-pi * 1 / 6) (400, 0)) Set.empty
                             , Finite (rotateV (-pi * 5 / 6) (400, 0)) Set.empty
                             ]
                }
  }

displayWorld :: World -> IO Picture
displayWorld = return . Pictures . ap ((:) . Circle . _spacing . _grid)
                                      (([drawGrid] <*>) . pure)

drawGrid :: World -> Picture
drawGrid world =
  Pictures
    $   [drawLimitPoints, drawGridLines (fromMaybe origin (world ^. bounds))]
    <*> (world ^. grid . limits)

drawLimitPoints :: Limit -> Picture
drawLimitPoints (Infinite _ _ _) = Blank
drawLimitPoints (Finite p _    ) = translateP p $ Circle 10

drawGridLines :: Bounds -> Limit -> Picture
drawGridLines b (Infinite a v ds) =
  Pictures $ [lineAND b a v] <*> Set.toList ds
drawGridLines b (Finite p as) = Pictures $ [linePA b p] <*> Set.toList as

translateP :: Point -> Picture -> Picture
translateP = uncurry Translate

rayPA :: Bounds -> Point -> Float -> Picture
-- draw ray based on point and angle
rayPA (w, h) p a =
  translateP p . (Rotate (radToDeg (-a))) $ Line [origin, (w + h, 0)]

lineAND :: Bounds -> Angle -> Vector -> Float -> Picture
-- draw line based on angle, normal vector, and distance
lineAND b a v d = linePA b (d Pt.* v) a

linePA :: Bounds -> Point -> Angle -> Picture
-- draw line based on point and angle
linePA (w, h) p a =
  translateP p . (Rotate (radToDeg (-a))) $ Line [(-w - h, 0), (w + h, 0)]

addLine :: Point -> Limit -> Limit
addLine q (Infinite a v ds) = Infinite a v (Set.insert (dotV q v) ds)
addLine q (Finite p as    ) = Finite p (Set.insert (argV (p Pt.- q)) as)

addGridPoint :: Point -> World -> World
addGridPoint p = over (grid . limits) ([addLine p] <*>)

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton LeftButton) Down undefined p) =
  return . addGridPoint p
handleInputs _ = return

timeUpdate :: Float -> World -> IO World
timeUpdate dt world = do
  let world' = (grid . spacing %~ (+ (dt * 0))) world
  if _bounds world == Nothing
    then do
      setBounds . addGridPoint origin $ world'
    else do
      return world'

setBounds :: World -> IO World
setBounds world = do
  gs <- getScreenSize
  let s = Just . join bimap fromIntegral $ gs
  return . set bounds s $ world
