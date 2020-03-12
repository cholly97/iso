module Geom where

import           Utils

import           Data.Fixed
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Geometry.Line

type Bounds = (Float, Float, Float, Float)

origin :: Point
origin = (0, 0)

dist :: Point -> Point -> Float
dist = magV <-<< (Pt.-)

projectVV :: Vector -> Vector -> Float
projectVV = dotV

anglePP :: Point -> Point -> Float
anglePP = mod' pi <-< argV <-<< (Pt.-)

pointPA :: Point -> Float -> Point
pointPA = unitVectorAtAngle >-<> (Pt.+)

translateP :: Point -> Picture -> Picture
translateP = uncurry Translate

linePP :: Bounds -> (Point, Point) -> Picture
linePP = intersectEdgesLine >-> uncurry >>-> Line

intersectLinesLines :: [(Point, Point)] -> [(Point, Point)] -> [Point]
intersectLinesLines ls ls' =
  catMaybes $ uncurry <$> uncurry intersectLineLine <$> ls <*> ls'

intersectLineSeg :: Point -> Point -> (Point, Point) -> Maybe Point
intersectLineSeg p1 p2 (p3, p4) = intersectSegLine p3 p4 p1 p2

intersectEdgesLine :: Bounds -> Point -> Point -> [Point]
-- should only have either 2 or 0, just think about it...
intersectEdgesLine b p q = catMaybes $ intersectLineSeg p q <$> edges b

edges :: Bounds -> [(Point, Point)]
edges (x0, x1, y0, y1) =
  [ ((x0, y0), (x0, y1))
  , ((x1, y0), (x1, y1))
  , ((x0, y0), (x1, y0))
  , ((x0, y1), (x1, y1))
  ]
