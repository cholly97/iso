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

type LineStore = Map.Map Float (Point, Point)

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

lookupNearestFunc
  :: Limit -> Float -> LineStore -> [Maybe (Float, (Point, Point))]
lookupNearestFunc (Infinite _ _ _) = lookupLGE
lookupNearestFunc (Finite _ _    ) = lookupLGECircular

lookupLGE :: Float -> LineStore -> [Maybe (Float, (Point, Point))]
lookupLGE k s = [Map.lookupLE, Map.lookupGE] <*> [k] <*> [s]

lookupLGECircular :: Float -> LineStore -> [Maybe (Float, (Point, Point))]
lookupLGECircular k s =
  firstMaybe
    <$> (?? s)
    <$> [[Map.lookupLE k, Map.lookupMax], [Map.lookupGE k, Map.lookupMin]]
  where firstMaybe = getFirst . mconcat . fmap First

calcSnapPoint :: Point -> [Limit] -> Maybe Point
calcSnapPoint p ls = getClosest p $ points
 where
  liness      = lookupNearest p <$> ls
  linesSuffs  = scanr (:) [] liness
  linesLiness = over _2 concat <$> mapMaybe uncons linesSuffs
  points      = concat $ uncurry intersectLinesLines <$> linesLiness

getClosest _ [] = Nothing
getClosest p qs = Just . minimumBy (compare `on` dist p) $ qs

pointToLineRep :: Point -> Limit -> (Float, (Point, Point))
pointToLineRep q (Infinite a v _) = (projectVV q v, (q, pointPA q a))
pointToLineRep q (Finite p _    ) = (anglePP q p, (q, p))

addLine :: Point -> Limit -> Limit
addLine q = over lineStore =<< uncurry Map.insert . pointToLineRep q
