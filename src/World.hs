{-# LANGUAGE TemplateHaskell #-}
module World where

import           Geom
import           Limit
import           Utils

import           Control.Conditional
import           Control.Lens
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Graphics.Gloss.Interface.Environment
import           Graphics.Gloss.Interface.IO.Game

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
