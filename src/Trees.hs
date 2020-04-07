{-# LANGUAGE FlexibleInstances #-}
module Trees where

import           Utils

import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe

data TreeView t a = Leaf | Node a (t a, t a)

getValue :: TreeView t a -> Maybe a
getValue (Node k _) = Just k
getValue _          = Nothing

instance Eq a => Eq (TreeView t a) where
  t1 == t2 = getValue t1 == getValue t2

instance Ord a => Ord (TreeView t a) where
  compare f1 f2 = compare (getValue f1) (getValue f2)

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

class BST bst where
  -- base case constructors
  -- W/S - O(1)
  empty :: bst a
  expose empty = Leaf
  unexpose Leaf = empty
  singleton :: a -> bst a
  -- expose $ singleton x = Node x (Leaf, Leaf)

  -- W/S - O(1)
  expose :: bst a -> TreeView bst a
  unexpose :: TreeView bst a -> bst a
  -- unexpose (expose t) = t
  -- expose (unexpose t) = t

  -- W/S - O(1)
  lift :: Finger bst a -> Maybe a
  lift (t, _, _) = getValue t

  -- W - O(|t| lg |t|)
  -- S - O(|t|)
  toList :: ([a] -> ([a], [a]) -> [a]) -> bst a -> [a]
  toList combine = traversal where
    traversal t = case expose t of
      Leaf          -> []
      Node k c -> combine [k] $ Control.Monad.join bimap traversal c
  inord, preord, postord :: bst a -> [a]
  preord = toList $ \x (l, r) -> x ++ l ++ r
  inord = toList $ \x (l, r) -> l ++ x ++ r
  postord = toList $ \x (l, r) -> l ++ r ++ x

  -- finger stuff
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
  inf' = doUntilNothing childL >-> parent >-> try "empty tree" id
  sup' = doUntilNothing childR >-> parent >-> try "empty tree" id
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
  searchFrom _ (Leaf, _, _) = Nothing
  searchFrom k f@(Node k' (l, r), i, _) = if inRange k i
    then case compare k k' of
      EQ -> Just f
      LT -> searchFrom k =<< childL f
      GT -> searchFrom k =<< childR f
    else searchFrom k =<< parent f
  -- alternate search where a Leaf finger is returned instead of Nothing
  -- W/S - O(lg |t|) expected
  search' :: Ord a => a -> bst a -> Finger bst a
  search' k t = maybe (Leaf, (NegInf, PosInf), []) (searchFrom' k) $ root t
  -- W/S - O(lg(j - i + 1)) expected
  -- where i, j are ranks of the start and end keys
  searchFrom' :: Ord a => a -> Finger bst a -> Finger bst a
  searchFrom' _ f@(Leaf, _, _) = f
  searchFrom' k f@(Node k' (l, r), i, _) = if inRange k i
    then case compare k k' of
      EQ -> f
      LT -> try "bad implementation of childL" cont $ childL f
      GT -> try "bad implementation of childR" cont $ childR f
    else try "malformed root finger, range /= (NegInf, PosInf)" cont $ parent f
    where cont = searchFrom' k

  toString :: Show a => bst a -> String
  toString t = case expose t of
    Leaf          -> ""
    Node k (l, r) -> mconcat ["(", toString l, show k, toString r, ")"]
