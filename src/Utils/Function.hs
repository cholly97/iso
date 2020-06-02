module Utils.Function where

import           Utils.Combinators

data Recursive a b = Ret b | Rec a

recurse :: (a -> Recursive a b) -> a -> b
recurse f k = case f k of
  Ret ret -> ret
  Rec k'  -> recurse f k'

syncStep
  :: (a -> Recursive a b) -> [Recursive a b] -> Recursive [Recursive a b] b
syncStep _ []           = Rec []
syncStep _ (Ret b : rs) = Ret b
syncStep f (Rec a : rs) = case syncStep f rs of
  Ret b  -> Ret b
  Rec as -> Rec (f a : as)

recurseSync :: (a -> Recursive a b) -> [a] -> b
recurseSync = syncStep >-> recurse <>--< map Rec

maybeRec :: Maybe a -> Recursive a (Maybe a)
maybeRec Nothing  = Ret Nothing
maybeRec (Just k) = Rec k
