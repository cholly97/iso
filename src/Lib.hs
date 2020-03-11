{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Geom
import           Limit

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment
import           Graphics.Gloss.Interface.IO.Game

data Grid = Grid
  { _spacing :: Float
  , _limits :: [Limit]
  }

$(makeLenses ''Grid)

data World = World
  { _bounds :: Maybe Bounds
  , _mousePos :: Point
  , _snapPoint :: Point
  , _grid :: Grid
  }

$(makeLenses ''World)

initWorld :: World
initWorld = World
  { _bounds    = Nothing
  , _mousePos  = (0, 0)
  , _snapPoint = (0, 0)
  , _grid      = Grid
                   { _spacing = 42.0
                   , _limits  = [ newInfiniteLimit (pi / 2)
                                , newFiniteLimit (-pi * 5 / 6) 400
                                , newFiniteLimit (-pi * 1 / 6) 400
                                ]
                   }
  }

displayWorld :: World -> IO Picture
displayWorld = return . Pictures . ap [drawSpacing, drawGrid] . pure

drawSpacing :: World -> Picture
drawSpacing = Circle . _spacing . _grid

gridLimits :: Getting [Limit] World [Limit]
gridLimits = grid . limits

drawGrid :: World -> Picture
drawGrid w =
  Pictures
    $ (drawPoint $ w ^. snapPoint)
    : ([drawLimitPoints, maybeDrawGridLines] <*> w ^. gridLimits)
 where
  maybeDrawGridLines = fromMaybe Blank . (drawGridLines <$> w ^. bounds ??)

drawLimitPoints :: Limit -> Picture
drawLimitPoints (Infinite _ _ _) = Blank
drawLimitPoints (Finite p _    ) = drawPoint p

drawPoint :: Point -> Picture
drawPoint p = Color blue . translateP p $ ThickCircle 2 4

drawGridLines :: Bounds -> Limit -> Picture
drawGridLines b = Pictures . (linePP b <$>) . Map.elems . view lineStore

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton LeftButton) Down m p) =
  return . (addGridPoint =<< pointFunc m)
 where
  pointFunc Modifiers { shift = Down } = view snapPoint
  pointFunc _                          = const p
handleInputs (EventMotion p) = return . set mousePos p
handleInputs _               = return

addGridPoint :: Point -> World -> World
addGridPoint p = grid . limits %~ (addLine p <$>)

timeUpdate :: Float -> World -> IO World
timeUpdate dt = maybeInit >=> return . setSnapPoint

setSnapPoint :: World -> World
setSnapPoint = set snapPoint =<< fromMaybe . view snapPoint <*> f
  where f = calcSnapPoint <$> view mousePos <*> view gridLimits

maybeInit :: World -> IO World
maybeInit w = if _bounds w == Nothing
  then do
    setBounds . addGridPoint origin $ w
  else do
    return w

setBounds :: World -> IO World
setBounds world = do
  wh <- getScreenSize
  let (w, h) = join bimap fromIntegral $ wh
  let s      = Just (-w / 2, w / 2, -h / 2, h / 2)
  return . set bounds s $ world
