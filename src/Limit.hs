{-# LANGUAGE TemplateHaskell #-}
module Limit where

import           Geom
import           Utils

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Function
import           Data.List                      ( minimumBy )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Monoid
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Line

data Stickiness = Stickiness
  { _line      :: Float
  , _intersect :: Float
  }

type Line = (Point, Point)

type LineStore = Map.Map Float Line

data Limit
  = Infinite {angle :: Float, normal :: Vector, _lineStore :: LineStore}
  | Finite {point :: Point, _lineStore :: LineStore}

$(makeLenses ''Limit)

infiniteLimit :: Float -> Limit
infiniteLimit a = Infinite a (unitVectorAtAngle (a + pi / 2)) Map.empty

finiteLimit :: Float -> Float -> Limit
finiteLimit a d = Finite (rotateV a (d, 0)) Map.empty

lookupNearest :: Point -> Limit -> [Line]
lookupNearest p =
  lookupNearestFunc
    <*> fst
    .   pointToLineRep p
    <*> _lineStore
    >-> catMaybes
    >-> fmap snd

lookupNearestFunc :: Limit -> Float -> LineStore -> [Maybe (Float, Line)]
lookupNearestFunc (Infinite _ _ _) = lookupLGE
lookupNearestFunc (Finite _ _    ) = lookupLGECircular

lookupLGE :: Float -> LineStore -> [Maybe (Float, Line)]
lookupLGE k s = [Map.lookupLE, Map.lookupGE] <*> [k] <*> [s]

lookupLGECircular :: Float -> LineStore -> [Maybe (Float, Line)]
lookupLGECircular k s =
  firstMaybe
    <$> flip flap s
    <$> [[Map.lookupLE k, Map.lookupMax], [Map.lookupGE k, Map.lookupMin]]
  where firstMaybe = fmap First >-> mconcat >-> getFirst

calcSnapPoint :: Point -> [Limit] -> Stickiness -> Point
calcSnapPoint p ls stick = compByFunc minimumBy points
 where
  liness :: [[Line]]
  liness = lookupNearest p <$> ls

  linePoints :: [Point]
  linePoints = uncurry closestPointOnLine <$> concat liness <*> [p]

  linesSuffs :: [[[Line]]]
  linesSuffs = scanr (:) [] liness
  linesLiness :: [([Line], [Line])]
  linesLiness = over _2 concat <$> mapMaybe uncons linesSuffs
  intersects :: [Point]
  intersects = concat $ uncurry intersectLinesLines <$> linesLiness

  mapOffset :: (Stickiness -> Float) -> [Point] -> [CompBy Point Float]
  mapOffset = eval stick >-> subtract <>-< dist p >-> map . CompBy

  points :: [CompBy Point Float]
  points =
    CompBy (const 0) p
      :  mapOffset _line      linePoints
      ++ mapOffset _intersect intersects

pointToLineRep :: Point -> Limit -> (Float, Line)
pointToLineRep q (Infinite a v _) = (projectVV q v, (q, pointPA q a))
pointToLineRep q (Finite p _    ) = (anglePP q p, (q, p))
