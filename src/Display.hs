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
    >->   _stickiness
    >->   flap [_line, _intersect]
    >>--> Circle
    >->   Pictures
    -<    moveToMouse
  where moveToMouse = _mousePos >-> translateP -< ap

drawGrid :: World -> Picture
drawGrid w = Pictures $ drawSnapPoint : drawLimits
 where
  drawLimits    = [drawLimitPoints, maybeDrawGridLines] <*> w ^. limits
  drawSnapPoint = drawPoint $ w ^. snapPoint
  maybeDrawGridLines =
    drawGridLines <>-< flap >- w ^. bounds >-> fromMaybe Blank

drawLimitPoints :: Limit -> Picture
drawLimitPoints Infinite{}   = Blank
drawLimitPoints (Finite p _) = drawPoint p

drawPoint :: Point -> Picture
drawPoint p = Color blue . translateP p $ ThickCircle 2 4

drawGridLines :: Bounds -> Limit -> Picture
drawGridLines =
--lim->ls            ls->[l]         p<-[l]<-([p]<-[l])         p<-l<-b
  view lineStore >-> Map.elems >--<> fmap Pictures <-< fmap <-< linePP
