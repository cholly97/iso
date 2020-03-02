{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Geom
import           Limit
import           Data.Maybe
import           Control.Lens
import           Control.Arrow
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Gloss.Interface.Environment

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
                       , _limits = [ Infinite (pi / 2) (1, 0) Map.empty
                                   , Finite (rotateV (-pi * 5 / 6) (400, 0)) Map.empty
                                   , Finite (rotateV (-pi * 1 / 6) (400, 0)) Map.empty
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
        <*> [fromMaybe (0, 0, 0, 0) $ world ^. bounds]
        )
    <*> world
    ^.  grid
    .   limits

drawLimitPoints :: Limit -> Picture
drawLimitPoints (Infinite _ _ _) = Blank
drawLimitPoints (Finite p _    ) = drawPoint p

drawPoint :: Point -> Picture
drawPoint p = Color blue $ translateP p $ Circle 2

drawGridLines :: Bounds -> Limit -> Picture
drawGridLines b l = Pictures $ linePP b <$> snd <$> Map.toList (l ^. lineStore)

drawClosestGridLines :: Point -> Bounds -> Limit -> Picture
drawClosestGridLines p b l =
  Pictures $ Color red . linePP b <$> snd <$> lookupNearest p l

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton LeftButton) Down undefined p) =
  return . addGridPoint p
handleInputs (EventMotion p) = return . set mousePosition p
handleInputs _               = return

addGridPoint :: Point -> World -> World
addGridPoint p = grid . limits %~ (addLine p <$>)

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
  let (w, h) = join bimap fromIntegral $ gs
  let s      = Just (-w / 2, w / 2, -h / 2, h / 2)
  return . set bounds s $ world
