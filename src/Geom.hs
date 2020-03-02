module Geom where

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

projectVV :: Vector -> Vector -> Float
projectVV = dotV

anglePP :: Point -> Point -> Float
anglePP p q = argV (p Pt.- q) `mod'` pi

pointPA :: Point -> Float -> Point
pointPA p a = (p Pt.+ unitVectorAtAngle a)

translateP :: Point -> Picture -> Picture
translateP = uncurry Translate

intersectLineSeg :: Point -> Point -> (Point, Point) -> Maybe Point
intersectLineSeg p1 p2 (p3, p4) = intersectSegLine p3 p4 p1 p2

linePP :: Bounds -> (Point, Point) -> Picture
linePP b (p, q) = Line $ intersectLineWithEdges b p q

intersectLineWithEdges :: Bounds -> Point -> Point -> [Point]
-- should only have either 2 or 0, just think about it...
intersectLineWithEdges b p q = catMaybes $ intersectLineSeg p q <$> edges b

edges :: Bounds -> [(Point, Point)]
edges (x0, x1, y0, y1) =
  [ ((x0, y0), (x0, y1))
  , ((x1, y0), (x1, y1))
  , ((x0, y0), (x1, y0))
  , ((x0, y1), (x1, y1))
  ]
