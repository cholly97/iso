{-# LANGUAGE TemplateHaskell #-}
module World where

import           Geom
import           Limit
import           Utils

import           Control.Conditional            ( if' )
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
  if' <$> _snapState <*> getSnapPoint <*> _mousePos >>= set snapPoint

getSnapPoint :: World -> Point
getSnapPoint = snap <$> _mousePos <*> _limits <*> _stickiness . _settings

maybeInit :: World -> IO World
maybeInit = doInit >- maybe -< const return <-< _bounds >- join

doInit :: World -> IO World
doInit w = getScreenSize >-> toBound . join bimap fromIntegral >>= fin
 where
  toBound (w, h) = Just (-w / 2, w / 2, -h / 2, h / 2)
  fin = set bounds ?? w >-> addGridPoint origin >-> return

modWorld :: ((Float, Line) -> LineStore -> LineStore) -> Point -> World -> World
-- modWorld f p w = over limits ?? w $ fmap g
  -- where g = over lineStore =<< f . pointToLineRep p
modWorld f =
  pointToLineRep >>--> f >>=> over lineStore >-> fmap >-> over limits

addGridPoint :: Point -> World -> World
addGridPoint = fuzzyInsert >- modWorld

removeGridPoint :: Point -> World -> World
removeGridPoint = fst >-> fuzzyDelete >- modWorld
