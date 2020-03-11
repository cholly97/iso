{-# LANGUAGE TemplateHaskell #-}
module Limit where

import           Geom

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

type Line = (Point, Point)

type LineStore = Map.Map Float Line

data Limit
  = Infinite {angle :: Float, normal :: Vector, _lineStore :: LineStore}
  | Finite {point :: Point, _lineStore :: LineStore}

$(makeLenses ''Limit)

newInfiniteLimit :: Float -> Limit
newInfiniteLimit a = Infinite a (unitVectorAtAngle (a + pi / 2)) Map.empty

newFiniteLimit :: Float -> Float -> Limit
newFiniteLimit a d = Finite (rotateV a (d, 0)) Map.empty

lookupNearest :: Point -> Limit -> [(Point, Point)]
lookupNearest p =
  lookupNearestFunc
    <*> fst
    .   pointToLineRep p
    <*> _lineStore
    >>> fmap snd
    .   catMaybes

lookupNearestFunc :: Limit -> Float -> LineStore -> [Maybe (Float, Line)]
lookupNearestFunc (Infinite _ _ _) = lookupLGE
lookupNearestFunc (Finite _ _    ) = lookupLGECircular

lookupLGE :: Float -> LineStore -> [Maybe (Float, Line)]
lookupLGE k s = [Map.lookupLE, Map.lookupGE] <*> [k] <*> [s]

lookupLGECircular :: Float -> LineStore -> [Maybe (Float, Line)]
lookupLGECircular k s =
  firstMaybe
    <$> (?? s)
    <$> [[Map.lookupLE k, Map.lookupMax], [Map.lookupGE k, Map.lookupMin]]
  where firstMaybe = getFirst . mconcat . fmap First

data CompBy a b = CompBy {compFunc :: (a -> b), value :: a}

compByFunc
  :: Ord b
  => ((CompBy a b -> CompBy a b -> Ordering) -> [CompBy a b] -> CompBy a b)
  -> [CompBy a b]
  -> a
compByFunc f = value . f (compare `on` compFunc <*> value)

calcSnapPoint :: Point -> [Limit] -> Float -> Float -> Point
calcSnapPoint p ls line intersect = compByFunc minimumBy points
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

  points :: [CompBy Point Float]
  points =
    (CompBy (const 0)) p
      : (  (CompBy ((subtract line) . dist p) <$> linePoints)
        ++ (CompBy ((subtract intersect) . dist p) <$> intersects)
        )

pointToLineRep :: Point -> Limit -> (Float, (Point, Point))
pointToLineRep q (Infinite a v _) = (projectVV q v, (q, pointPA q a))
pointToLineRep q (Finite p _    ) = (anglePP q p, (q, p))

addLine :: Point -> Limit -> Limit
addLine q = over lineStore =<< uncurry Map.insert . pointToLineRep q
