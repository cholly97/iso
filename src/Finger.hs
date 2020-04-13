module Finger where

import           Trees
import           Utils

import           Data.Maybe

data Range a = NegInf | PosInf | Finite a deriving Eq

instance Ord a => Ord (Range a) where
  compare NegInf     NegInf     = EQ
  compare PosInf     PosInf     = EQ
  compare NegInf     _          = LT
  compare _          PosInf     = LT
  compare PosInf     _          = GT
  compare _          NegInf     = GT
  compare (Finite x) (Finite y) = compare x y

inRange :: Ord a => a -> (Range a, Range a) -> Bool
inRange k (lo, hi) = (compare lo (Finite k), compare hi (Finite k)) == (LT, GT)

type Interval a = (Range a, Range a)
data Parent t a = L a (t a) (Range a) | R a (t a) (Range a)
type Finger t a = (TreeView t a, Interval a, [Parent t a])

class BST bst => FingerBST bst where
  -- W/S - O(1)
  lift :: Finger bst a -> Maybe a
  lift (t, _, _) = getValue t

  -- moving the finger around
  -- W/S - O(1)
  childL, childR, parent :: Finger bst a -> Maybe (Finger bst a)
  childL (t, (lo, hi), ps) = case t of
    Leaf          -> Nothing
    Node k (l, r) -> Just (expose l, (lo, Finite k), L k r hi : ps)
  childR (t, (lo, hi), ps) = case t of
    Leaf          -> Nothing
    Node k (l, r) -> Just (expose r, (Finite k, hi), R k l lo : ps)
  parent (t, (lo, hi), p : ps) = case p of
    L k r hi' -> Just (Node k (unexpose t, r), (lo, hi'), ps)
    R k l lo' -> Just (Node k (l, unexpose t), (lo', hi), ps)
  parent _ = Nothing
  -- constructing fingers
  -- root: W/S - O(1)
  -- inf, sup: W/S - (lg |t|)
  root, inf, sup :: bst a -> Maybe (Finger bst a)
  root b = case t of
    Leaf -> Nothing
    _ -> Just (t, (NegInf, PosInf), [])
    where t = expose b
  inf = doToRoot inf'
  sup = doToRoot sup'
  doToRoot :: (Finger bst a -> Finger bst a) -> bst a -> Maybe (Finger bst a)
  doToRoot = (root >>-->)
  inf', sup' :: Finger bst a -> Finger bst a
  inf' = doUntilNothing childL >-> parent >-> tryFromMaybe "empty tree"
  sup' = doUntilNothing childR >-> parent >-> tryFromMaybe "empty tree"
  -- reconstructing trees
  -- W/S - O(lg |t|)
  reconstruct :: Finger bst a -> bst a
  reconstruct f@(t, _, _) = maybe (unexpose t) reconstruct $ parent f
  -- rotations
  -- W/S - O(1)
  rotateR :: Finger bst a -> Maybe (Finger bst a)
  rotateR (Leaf, _, _) = Nothing
  rotateR (Node k (l, r), i, ps) =
    case expose l of
      Leaf -> Nothing
      Node lk (ll, lr) -> Just (Node lk (ll, r'), i, ps)
        where r' = unexpose $ Node k (lr, r)
  rotateL :: Finger bst a -> Maybe (Finger bst a)
  rotateL (Leaf, _, _) = Nothing
  rotateL (Node k (l, r), i, ps) =
    case expose r of
      Leaf -> Nothing
      Node rk (rl, rr) -> Just (Node rk (l', rr), i, ps)
        where l' = unexpose $ Node k (l, rl)
  -- iterating using fingers
  -- W/S - O(1) expected
  next, prev :: Ord a => Finger bst a -> Maybe (Finger bst a)
  next = adj inf' childR parentR
  prev = adj sup' childL parentL
  adj :: Ord a =>
    (Finger bst a -> Finger bst a) ->
    (Finger bst a -> Maybe (Finger bst a)) ->
    (Finger bst a -> Maybe (Finger bst a)) ->
    Finger bst a -> Maybe (Finger bst a)
  adj b' c p f =
    if isNothing child then p f else child where child = c f >-> b'
  parentL, parentR :: Finger bst a -> Maybe (Finger bst a)
  parentL f@(_, _, p : _) = case p of
    L {} -> parent f
    R {} -> parent f >>= parentL
  parentL _ = Nothing
  parentR f@(_, _, p : _) = case p of
    L {} -> parent f >>= parentR
    R {} -> parent f
  parentR _ = Nothing

  -- searching a tree
  -- W/S - O(lg |t|) expected
  search :: Ord a => a -> bst a -> Maybe (Finger bst a)
  search k = root >>=> searchFrom k
  -- searching from a finger
  -- W/S - O(lg(j - i + 1)) expected
  -- where i, j are ranks of the start and end keys
  searchFrom :: Ord a => a -> Finger bst a -> Maybe (Finger bst a)
  searchFrom k = sf where
    sf f@(tv, i, _) = case tv of
      Leaf -> Nothing
      Node k' _ -> if inRange k i
        then case compare k k' of
          EQ -> Just f
          LT -> sf =<< childL f
          GT -> sf =<< childR f
        else sf =<< parent f
  -- alternate search where a Leaf finger is returned instead of Nothing
  -- W/S - O(lg |t|) expected
  search' :: Ord a => a -> bst a -> Finger bst a
  search' k t = maybe (Leaf, (NegInf, PosInf), []) (searchFrom' k) $ root t
  -- W/S - O(lg(j - i + 1)) expected
  -- where i, j are ranks of the start and end keys
  searchFrom' :: Ord a => a -> Finger bst a -> Finger bst a
  searchFrom' k = sf where
    sf f@(tv, i, _) = case tv of
      Leaf -> f
      Node k' _ -> if inRange k i
        then case compare k k' of
          EQ -> f
          LT -> tryMaybe "bad implementation of childL" sf $ childL f
          GT -> tryMaybe "bad implementation of childR" sf $ childR f
        else tryMaybe "malformed root finger" sf $ parent f

  -- last non-leaf node accessed, unless tree is empty
  -- W/S - O(1)
  lastAccessed :: Finger bst a -> Finger bst a
  lastAccessed f@(Leaf, _, _:_) = tryFromMaybe "no parent" $ parent f
  lastAccessed f = f
