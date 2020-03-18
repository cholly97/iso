{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Geom
import           Limit
import           Utils

import           Control.Arrow
import           Control.Applicative
import           Control.Conditional
import           Control.Lens
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment
import           Graphics.Gloss.Interface.IO.Game
import           System.IO

data Settings = Settings
  { _stickiness :: Stickiness
  }

data World = World
  { _bounds    :: Maybe Bounds
  , _mousePos  :: Point
  , _snapPoint :: Point
  , _snapState :: Bool
  , _settings  :: Settings
  , _limits    :: [Limit]
  }
$(makeLenses ''World)

initWorld :: World
initWorld = World
  { _bounds    = Nothing
  , _mousePos  = (0, 0)
  , _snapPoint = (0, 0)
  , _snapState = False
  , _settings  = Settings
                   { _stickiness = Stickiness { _line = 10, _intersect = 20 }
                   }
  , _limits    = [ infiniteLimit (pi / 2)
                 , finiteLimit (-pi * 5 / 6) 400
                 , finiteLimit (-pi * 1 / 6) 400
                 ]
  }

displayWorld :: World -> IO Picture
displayWorld = flap [drawRanges, drawGrid] >-> Pictures >-> return

drawRanges :: World -> Picture
drawRanges =
  _settings
    >-> _stickiness
    >-> flap [_line, _intersect]
    >-> fmap Circle
    >-> Pictures
    -<  moveToMouse
  where moveToMouse = _mousePos >-> translateP -< ap

drawGrid :: World -> Picture
drawGrid w =
  Pictures
    $ (drawPoint >- w ^. snapPoint)
    : ([drawLimitPoints, maybeDrawGridLines] <*> w ^. limits)
 where
  maybeDrawGridLines =
    flap <-< fmap drawGridLines >- w ^. bounds >-> fromMaybe Blank

drawLimitPoints :: Limit -> Picture
drawLimitPoints (Infinite _ _ _) = Blank
drawLimitPoints (Finite p _    ) = drawPoint p

drawPoint :: Point -> Picture
drawPoint p = Color blue . translateP p $ ThickCircle 2 4

drawGridLines :: Bounds -> Limit -> Picture
drawGridLines =
--lim->ls            ls->[l]        p<-x<-([p]<-x)    [p]<-[l]<-b
  view lineStore >-> Map.elems >-<> fmap Pictures <-< fmap <-< linePP

handleInputs :: Event -> World -> IO World
handleInputs (EventKey (MouseButton button) Down m p) = do
  view limits >-> fmap _lineStore >-> length >-> show >-> putStrLn
  pointFunc m >-> show >-> putStrLn
  pointFunc m >>= operation button >-> return
 where
  operation LeftButton  = addGridPoint
  operation RightButton = removeGridPoint
  pointFunc Modifiers { shift = Down } = view snapPoint
  pointFunc _                          = const p
handleInputs (EventKey (SpecialKey KeyShiftL) s _ _) =
  s == Down -< set snapState >-> return
handleInputs (EventMotion p) = set mousePos p >-> return
handleInputs _               = return

timeUpdate :: Float -> World -> IO World
timeUpdate dt = maybeInit >=> return . setSnapPoint

setSnapPoint :: World -> World
setSnapPoint =
  return if' <*> _snapState <*> getSnapPoint <*> _mousePos >>= set snapPoint
 where
  getSnapPoint = snap <$> _mousePos <*> _limits <*> _stickiness . _settings

maybeInit :: World -> IO World
maybeInit = doInit -< maybe >- const return <-< _bounds -< join

doInit :: World -> IO World
doInit world = do
  wh <- getScreenSize
  let (w, h) = join bimap fromIntegral $ wh
  let s      = Just (-w / 2, w / 2, -h / 2, h / 2)
  world -< set bounds s -< addGridPoint origin -< return

modWorld :: ((Float, Line) -> LineStore -> LineStore) -> Point -> World -> World
modWorld f = pointToLineRep >>-> f >>=> over lineStore >-> fmap >-> over limits

addGridPoint :: Point -> World -> World
addGridPoint = uncurry Map.insert -< modWorld

removeGridPoint :: Point -> World -> World
removeGridPoint = fst >-> Map.delete -< modWorld
