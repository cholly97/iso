module Geom where

import           Data.Fixed
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Graphics.Gloss.Geometry.Angle

type Bounds = (Float, Float)

origin :: Point
origin = (0, 0)

projectVV :: Vector -> Vector -> Float
projectVV = dotV

anglePP :: Point -> Point -> Float
anglePP p q = argV (p Pt.- q) `mod'` pi

translateP :: Point -> Picture -> Picture
translateP = uncurry Translate

rayPA :: Bounds -> Point -> Float -> Picture
-- draw ray based on point and angle
rayPA (w, h) p a =
  translateP p . (Rotate . radToDeg $ -a) $ Line [origin, (w + h, 0)]

lineAND :: Bounds -> Float -> Vector -> Float -> Picture
-- draw line based on angle, normal vector, and distance
lineAND b a v d = linePA b (d Pt.* v) a

linePA :: Bounds -> Point -> Float -> Picture
-- draw line based on point and angle
linePA (w, h) p a =
  translateP p . (Rotate . radToDeg $ -a) $ Line [(-w - h, 0), (w + h, 0)]
