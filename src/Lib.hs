{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Geom
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Monoid
import           Control.Lens
import           Control.Arrow
import           Control.Monad
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Gloss.Interface.Environment


data Limit
  = Infinite {angle :: Float, normal :: Vector, _lineStore :: Set.Set Float}
  | Finite {point :: Point, _lineStore :: Set.Set Float}

$(makeLenses ''Limit)

data Grid = Grid
  { _spacing :: Float
  , _limits :: [Limit]
  }

$(makeLenses ''Grid)

data World = World
  { _bounds :: Maybe Bounds
  , _mousePosition :: Point
  , _grid :: Grid
  }

$(makeLenses ''World)

initWorld :: World
initWorld = World
  { _bounds        = Nothing
  , _mousePosition = (0, 0)
  , _grid          = Grid
                       { _spacing = 42.0
                       , _limits = [ Infinite (pi / 2) (1, 0) Set.empty
                                   , Finite (rotateV (-pi * 5 / 6) (400, 0)) Set.empty
                                   , Finite (rotateV (-pi * 1 / 6) (400, 0)) Set.empty
                                   ]
                       }
  }

displayWorld :: World -> IO Picture
displayWorld = return . Pictures . ap ((:) . Circle . _spacing . _grid)
                                      (([drawGrid] <*>) . pure)

drawGrid :: World -> Picture
drawGrid world =
  Pictures
    $   [drawLimitPoints]
    ++  (   [drawGridLines, drawClosestGridLines $ world ^. mousePosition]
        <*> [fromMaybe origin $ world ^. bounds]
        )
    <*> world
    ^.  grid
    .   limits

lineFunc :: Bounds -> Limit -> Float -> Picture
lineFunc b (Infinite a v _) = lineAND b a v
lineFunc b (Finite p _    ) = linePA b p

lookupNearest :: Ord a => Limit -> a -> Set.Set a -> [a]
lookupNearest (Infinite _ _ _) = lookupLGE
lookupNearest (Finite _ _    ) = lookupLGECircular

pointToLineRep :: Point -> Limit -> Float
pointToLineRep q (Infinite _ v _) = projectVV q v
pointToLineRep q (Finite p _    ) = anglePP q p

drawClosestGridLines :: Point -> Bounds -> Limit -> Picture
drawClosestGridLines q b l =
  Pictures $ [Color red . lineFunc b l] <*> (lookupNearest l)
    (pointToLineRep q l)
    (_lineStore l)

lookupLGE :: Ord a => a -> Set.Set a -> [a]
lookupLGE k s = catMaybes $ [Set.lookupLE, Set.lookupGE] <*> [k] <*> [s]

lookupLGECircular :: Ord a => a -> Set.Set a -> [a]
lookupLGECircular k s =
  catMaybes
    $   [getFirst . mconcat . map First]
    <*> (   [(<*> [s])]
        <*> [[Set.lookupLE k, Set.lookupMax], [Set.lookupGE k, Set.lookupMin]]
        )

drawLimitPoints :: Limit -> Picture
drawLimitPoints (Infinite _ _ _) = Blank
drawLimitPoints (Finite p _    ) = translateP p $ Circle 10

drawGridLines :: Bounds -> Limit -> Picture
drawGridLines b l = Pictures $ [lineFunc b l] <*> Set.toList (_lineStore l)

addLine :: Point -> Limit -> Limit
addLine q l = (lineStore %~ Set.insert (pointToLineRep q l)) l

addGridPoint :: Point -> World -> World
addGridPoint p = grid . limits %~ ([addLine p] <*>)

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton LeftButton) Down undefined p) =
  return . addGridPoint p
handleInputs (EventMotion p) = return . set mousePosition p
handleInputs _               = return

timeUpdate :: Float -> World -> IO World
timeUpdate dt world = do
  if _bounds world == Nothing
    then do
      setBounds . addGridPoint origin $ world
    else do
      return world

setBounds :: World -> IO World
setBounds world = do
  gs <- getScreenSize
  let s = Just . join bimap fromIntegral $ gs
  return . set bounds s $ world
