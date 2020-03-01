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
  = Infinite {angle :: Angle, normal :: Vector, dists :: Set.Set Float}
  | Finite {point :: Point, angles :: Set.Set Angle}

data Grid = Grid
  { _spacing :: Float
  , _limits :: [Limit]
  }

data World = World
  { _bounds :: Maybe Bounds
  , _mousePosition :: Point
  , _grid :: Grid
  }

$(makeLenses ''World)
$(makeLenses ''Grid)

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

drawClosestGridLines :: Point -> Bounds -> Limit -> Picture
drawClosestGridLines q b (Infinite a v ds) =
  Pictures $ [Color red . lineAND b a v] <*> lookupLGE (projectVV q v) ds
drawClosestGridLines q b (Finite p as) =
  Pictures $ [Color red . linePA b p] <*> lookupLGECircular (anglePP p q) as

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
drawGridLines b (Infinite a v ds) =
  Pictures $ [lineAND b a v] <*> Set.toList ds
drawGridLines b (Finite p as) = Pictures $ [linePA b p] <*> Set.toList as

addLine :: Point -> Limit -> Limit
addLine q (Infinite a v ds) = Infinite a v (Set.insert (projectVV q v) ds)
addLine q (Finite p as    ) = Finite p (Set.insert (anglePP p q) as)

addGridPoint :: Point -> World -> World
addGridPoint p = over (grid . limits) ([addLine p] <*>)

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton LeftButton) Down undefined p) =
  return . addGridPoint p
handleInputs (EventMotion p) = return . set mousePosition p
handleInputs _               = return

timeUpdate :: Float -> World -> IO World
timeUpdate dt world = do
  let world' = (grid . spacing %~ (+ (dt * 0))) world
  if _bounds world == Nothing
    then do
      setBounds . addGridPoint origin $ world'
    else do
      return world'

setBounds :: World -> IO World
setBounds world = do
  gs <- getScreenSize
  let s = Just . join bimap fromIntegral $ gs
  return . set bounds s $ world
