{-# LANGUAGE TemplateHaskell #-}
module Limit where

import           Geom
import           Data.Maybe
import           Data.Monoid
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

lookupNearest :: Ord a => Limit -> a -> Set.Set a -> [a]
lookupNearest (Infinite _ _ _) = lookupLGE
lookupNearest (Finite _ _    ) = lookupLGECircular

pointToLineRep :: Point -> Limit -> Float
pointToLineRep q (Infinite _ v _) = projectVV q v
pointToLineRep q (Finite p _    ) = anglePP q p

lookupLGE :: Ord a => a -> Set.Set a -> [a]
lookupLGE k s = catMaybes $ [Set.lookupLE, Set.lookupGE] <*> [k] <*> [s]

lookupLGECircular :: Ord a => a -> Set.Set a -> [a]
lookupLGECircular k s =
  catMaybes
    $   [getFirst . mconcat . map First]
    <*> (   [(<*> [s])]
        <*> [[Set.lookupLE k, Set.lookupMax], [Set.lookupGE k, Set.lookupMin]]
        )
