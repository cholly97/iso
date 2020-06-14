{-# LANGUAGE TemplateHaskell #-}
module Structs.SplayMap where

import           Structs.Finger
import qualified Structs.SelfBalancing         as SB
import           Structs.Splay
import           Structs.Trees
import           Utils.Combinators
import           Utils.Function

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe

data Entry k v = Entry {key :: k, val :: v}

dummy :: k -> Entry k v
dummy = flip Entry undefined

instance Eq k => Eq (Entry k v) where
  Entry k1 _ == Entry k2 _ = k1 == k2

instance Ord k => Ord (Entry k v) where
  compare (Entry k1 _) (Entry k2 _) = compare k1 k2

data SplayMap k v = SplayMap
  { _pointer, _pointerMin, _pointerMax :: Finger SplayTree (Entry k v)
  }
$(makeLenses ''SplayMap)

class Map m where
  empty :: m k v

  assocs :: m k v -> [(k, v)]
  keys :: m k v -> [k]
  elems :: m k v -> [v]

  insert :: Ord k => k -> v -> State (m k v) ()
  delete :: Ord k => k -> State (m k v) ()
  find :: Ord k => k -> m k v -> Bool

  lookup :: Ord k => k -> State (m k v) (Maybe v)
  lookupLGE :: Ord k => k -> State (m k v) [v]
  lookupLGECircular :: Ord k => k -> State (m k v) [v]

mapEntries :: (Entry k v -> a) -> SplayMap k v -> [a]
mapEntries = view pointer >-> reconstruct >-> inord >--<> map

fFinger
  :: (SplayTree a -> SplayTree a) -> Finger SplayTree a -> Finger SplayTree a
fFinger f = reconstruct >-> f >-> point'

fSplayMap
  :: (SplayTree (Entry k v) -> SplayTree (Entry k v))
  -> SplayMap k v
  -> SplayMap k v
fSplayMap = fFinger >-> over pointer >>--> doBounds

doBounds :: SplayMap k v -> SplayMap k v
doBounds (SplayMap f _ _) = SplayMap f (minimal' f) (maximal' f)

look (tv, (lo, hi), _) = case tv of
  Leaf     -> catMaybes [lo, hi] >-> val
  Node e _ -> [val e]

instance Map SplayMap where
  empty  = let e = point' Structs.Trees.empty in SplayMap e e e

  assocs = mapEntries -< liftM2 (,) key val
  keys   = mapEntries key
  elems  = mapEntries val

  insert = Entry >>--> SB.insert >>--> fSplayMap >>--> modify
  delete = dummy >-> SB.delete >-> fSplayMap >-> modify
  find k sm = SB.find -< dummy k -< reconstruct (_pointer sm)

  lookup key = state $ \sm ->
    let f      = searchFrom' -< dummy key -< _pointer sm
        result = case lift f of
          Leaf     -> Nothing
          Node e _ -> Just (val e)
    in  (result, set pointer f sm)

  lookupLGE key = state $ \sm ->
    let f = searchFrom' -< dummy key -< _pointer sm
    in  (look f, set pointer f sm)

  lookupLGECircular key = state $ \sm@(SplayMap p pmin pmax) ->
    let f = searchStep' -< dummy key >- recurseSync -< [p, pmin, pmax]
    in  (look f, set pointer f sm)

insert', delete' :: Ord k => Entry k v -> State (SplayMap k v) ()
insert' = SB.insert >-> fSplayMap >-> modify
delete' = SB.delete >-> fSplayMap >-> modify
