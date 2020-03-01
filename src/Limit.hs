{-# LANGUAGE TemplateHaskell #-}
module Limit where

import           Geom
import           Data.Maybe
import           Data.Monoid
import           Control.Monad
import qualified Data.Set                      as Set
import           Control.Lens
import           Graphics.Gloss

data Limit
  = Infinite {angle :: Float, normal :: Vector, _lineStore :: Set.Set Float}
  | Finite {point :: Point, _lineStore :: Set.Set Float}

$(makeLenses ''Limit)

lineFunc :: Bounds -> Limit -> Float -> Picture
lineFunc b (Infinite a v _) = lineAND b a v
lineFunc b (Finite p _    ) = linePA b p

lookupNearest :: Point -> Limit -> [Float]
lookupNearest p = lookupNearestFunc <*> pointToLineRep p <*> _lineStore

lookupNearestFunc :: Limit -> Float -> Set.Set Float -> [Float]
lookupNearestFunc (Infinite _ _ _) = lookupLGE
lookupNearestFunc (Finite _ _    ) = lookupLGECircular

lookupLGE :: Float -> Set.Set Float -> [Float]
lookupLGE k s = catMaybes $ [Set.lookupLE, Set.lookupGE] <*> [k] <*> [s]

lookupLGECircular :: Float -> Set.Set Float -> [Float]
lookupLGECircular k s =
  catMaybes
    $   [getFirst . mconcat . map First]
    <*> (   [(<*> [s])]
        <*> [[Set.lookupLE k, Set.lookupMax], [Set.lookupGE k, Set.lookupMin]]
        )

pointToLineRep :: Point -> Limit -> Float
pointToLineRep q (Infinite _ v _) = projectVV q v
pointToLineRep q (Finite p _    ) = anglePP q p
