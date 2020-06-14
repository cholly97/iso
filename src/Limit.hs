{-# LANGUAGE TemplateHaskell #-}
module Limit where

import           Geom
import           Structs.SplayMap
import           Utils.Combinators
import           Utils.Comparison
import           Utils.FuzzyFloat
import           Utils.State

import           Control.Lens
import           Control.Monad.Trans.State
import           Data.List                      ( minimumBy )
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Line

data Stickiness = Stickiness
  { _line      :: Float
  , _intersect :: Float
  }

type Line = (Point, Point)

type LineStore = SplayMap FuzzyFloat Line

data Limit
  = Infinite {angle :: Float, normal :: Vector, _lineStore :: LineStore}
  | Finite {point :: Point, _lineStore :: LineStore}

$(makeLenses ''Limit)

infiniteLimit :: Float -> Limit
infiniteLimit a = Infinite a -< unitVectorAtAngle (a + pi / 2) -< empty

finiteLimit :: Float -> Float -> Limit
finiteLimit a d = Finite -< rotateV a (d, 0) -< empty

lookupNearest :: Point -> State Limit [Line]
lookupNearest p =
  let stateLSL = lookupNearestFunc <*> FuzzyFloat . fst . pointToLineRep p
  in  fState (view lineStore) (over lineStore) stateLSL

lookupNearestFunc :: Limit -> FuzzyFloat -> State LineStore [Line]
lookupNearestFunc Infinite{} = lookupLGE
lookupNearestFunc Finite{}   = lookupLGECircular

snap :: Point -> Stickiness -> State [Limit] Point
snap p stick = state $ \ls ->
  let liness :: [[Line]]
      ls' :: [Limit]
      (liness, ls') = runState -< lookupNearest p <-< ls >- unzip

      linePoints :: [Point]
      linePoints = uncurry closestPointOnLine <$> concat liness <*> [p]

      linesSuffs :: [[[Line]]]
      linesSuffs = scanr (:) [] liness
      linesLiness :: [([Line], [Line])]
      linesLiness = over _2 concat <$> mapMaybe uncons linesSuffs
      intersects :: [Point]
      intersects = concat $ uncurry intersectLinesLines <$> linesLiness

      mapOffset :: (Stickiness -> Float) -> [Point] -> [CompBy Point Float]
      mapOffset sett = map . CompBy $ \q -> dist p q - sett stick
      -- mapOffset = stick ->> subtract <>--< dist p >-> map . CompBy

      points :: [CompBy Point Float]
      points =
          CompBy (const 0) p
            :  mapOffset _line      linePoints
            ++ mapOffset _intersect intersects
  in  (compByFunc minimumBy points, ls')

pointToLineRep :: Point -> Limit -> (Float, Line)
pointToLineRep q (Infinite a v _) = (projectVV q v, (q, pointPA q a))
pointToLineRep q (Finite p _    ) = (anglePP q p, (q, p))
