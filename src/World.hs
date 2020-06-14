{-# LANGUAGE TemplateHaskell #-}
module World where

import           Geom
import           Limit
import           Structs.SplayMap
import           Utils.Combinators
import           Utils.FuzzyFloat
import           Utils.State

import           Control.Conditional            ( if' )
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State
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
setSnapPoint = uncurry -< set snapPoint <-< pw
 where
  pw = if' <$> _snapState <*> runState getSnapPoint <*> liftM2 (,) _mousePos id

getSnapPoint :: State World Point
getSnapPoint = fState -< view limits -< over limits -< snapWorld
  where snapWorld = snap <$> _mousePos <*> _stickiness . _settings

maybeInit :: World -> IO World
maybeInit = doInit >- maybe -< const return <-< _bounds >- join

doInit :: World -> IO World
doInit w = getScreenSize >-> toBound . join bimap fromIntegral >>= fin
 where
  toBound (w, h) = Just (-w / 2, w / 2, -h / 2, h / 2)
  fin = set bounds ?? w >-> addGridPoint origin >-> return

modWorld
  :: (Entry FuzzyFloat Line -> State LineStore ()) -> Point -> World -> World
modWorld f p = over limits . fmap $ over lineStore =<< execf
 where
  execf = pointToLineRep p >-> fst >-> FuzzyFloat >-> dummy >-> f >-> execState

addGridPoint :: Point -> World -> World
addGridPoint = insert' >- modWorld

removeGridPoint :: Point -> World -> World
removeGridPoint = delete' >- modWorld
