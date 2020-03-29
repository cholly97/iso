module Display where

import           Geom
import           Limit
import           Utils
import           World

import           Control.Lens
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Graphics.Gloss.Interface.IO.Game

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
--lim->ls            ls->[l]         p<-x<-([p]<-x)    [p]<-[l]<-b
  view lineStore >-> Map.elems >--<> fmap Pictures <-< fmap <-< linePP
