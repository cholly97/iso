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

maxInterval :: Interval a
maxInterval = (NegInf, PosInf)

inRange :: Ord a => a -> (Range a, Range a) -> Bool
inRange k (lo, hi) = (compare lo (Finite k), compare hi (Finite k)) == (LT, GT)

type Interval a = (Range a, Range a)
data Parent t a = L (t a) (Range a) | R (t a) (Range a)
type Finger t a = (TreeView t a, Interval a, [Parent t a])

lift :: Finger t a -> TreeView t a
lift (t, _, _) = t
interval :: Finger t a -> Interval a
interval (_, i, _) = i
parents :: Finger t a -> [Parent t a]
parents (_, _, ps) = ps

class BST bst => FingerBST bst where
  -- constructing fingers
  point :: bst a -> Finger bst a
  point b = (expose b, maxInterval, [])
  -- reconstructing trees
  -- W/S - O(1)
  unPoint :: Finger bst a -> bst a
  unPoint = lift >-> unexpose
  -- W/S - O(lg |t|)
  reconstruct :: Finger bst a -> bst a
  reconstruct f@(t, _, _) = maybe (unexpose t) reconstruct $ parent f

  -- moving the finger around, fail if can't move
  -- W/S - O(1)
  parent, childL, childR :: Finger bst a -> Maybe (Finger bst a)
  parent (t, (lo, Finite k), L r hi' : ps) =
    Just (Node k (unexpose t, r), (lo, hi'), ps)
  parent (t, (Finite k, hi), R l lo' : ps) =
    Just (Node k (l, unexpose t), (lo', hi), ps)
  parent _ = Nothing
  childL (t, (lo, hi), ps) = case t of
    Leaf          -> Nothing
    Node k (l, r) -> Just (expose l, (lo, Finite k), L r hi : ps)
  childR (t, (lo, hi), ps) = case t of
    Leaf          -> Nothing
    Node k (l, r) -> Just (expose r, (Finite k, hi), R l lo : ps)

  -- bulk moves, fail if tree is empty
  -- root: W/S - O(1)
  -- minimal, maximal: W/S - (lg |t|)
  root, minimal, maximal :: Finger bst a -> Maybe (Finger bst a)
  root f@(Node {}, _, []) = Just f
  root f = parent f >>= root
  minimal f@(_, (NegInf, _), _) = inf f
  minimal f = parent f >>= minimal
  maximal f@(_, (_, PosInf), _) = sup f
  maximal f = parent f >>= maximal
  -- minimal/maximal in subtree, unless Leaf node, in which case return parent
  inf, sup :: Finger bst a -> Maybe (Finger bst a)
  inf = doUntilNothing childL >-> parent
  sup = doUntilNothing childR >-> parent

  -- rotations, fail if either of two nodes are Leafs
  -- W/S - O(1)
  rotateR, rotateL :: Finger bst a -> Maybe (Finger bst a)
  rotateR (Leaf, _, _) = Nothing
  rotateR (Node k (l, r), i, ps) =
    case expose l of
      Leaf -> Nothing
      Node lk (ll, lr) -> Just (Node lk (ll, r'), i, ps)
        where r' = unexpose $ Node k (lr, r)
  rotateL (Leaf, _, _) = Nothing
  rotateL (Node k (l, r), i, ps) =
    case expose r of
      Leaf -> Nothing
      Node rk (rl, rr) -> Just (Node rk (l', rr), i, ps)
        where l' = unexpose $ Node k (l, rl)

  -- iterating using fingers, fail if doesn't exist
  -- W/S - O(1) expected
  prev, next :: Ord a => Finger bst a -> Maybe (Finger bst a)
  prev f@(Leaf, _, _) = parentL f
  prev f@(Node k (l, _), _, _) = case expose l of
    Leaf -> parentL f
    _ -> childL f >>= sup
  next f@(Leaf, _, _) = parentR f
  next f@(Node k (_, r), _, _) = case expose r of
    Leaf -> parentR f
    _ -> childR f >>= inf
  parentL, parentR :: Finger bst a -> Maybe (Finger bst a)
  parentL f@(_, (_, Finite _), p : _) = case p of
    L {} -> parent f
    R {} -> parent f >>= parentL
  parentL _ = Nothing
  parentR f@(_, (Finite _, _), p : _) = case p of
    L {} -> parent f >>= parentR
    R {} -> parent f
  parentR _ = Nothing

  -- searching a tree, returns Leaf finger if not in tree
  -- W/S - O(lg |t|) expected
  search :: Ord a => a -> bst a -> Maybe (Finger bst a)
  search k = point >-> searchFrom k
  -- searching from a finger
  -- W/S - O(lg(j - i + 1)) expected
  -- where i, j are ranks of the start and end keys
  searchFrom :: Ord a => a -> Finger bst a -> Maybe (Finger bst a)
  searchFrom k = sf where
    sf f@(tv, i, _) = case tv of
      Leaf -> if inRange k i then Just f else parent f >>= sf
      Node k' _ -> if inRange k i
        then case compare k k' of
          EQ -> Just f
          LT -> childL f >>= sf
          GT -> childR f >>= sf
        else parent f >>= sf

  -- last non-leaf node accessed, unless tree is empty
  -- W/S - O(1)
  lastAccessed :: Finger bst a -> Maybe (Finger bst a)
  lastAccessed f@(Leaf, _, _) = parent f
  lastAccessed f = Just f

-- version that fails with error instead of using the Maybe monad
class BST bst => FingerBST' bst where
  -- constructing fingers
  point' :: bst a -> Finger bst a
  point' b = (expose b, maxInterval, [])
  -- reconstructing trees
  -- W/S - O(1)
  unPoint' :: Finger bst a -> bst a
  unPoint' = lift >-> unexpose
  -- W/S - O(lg |t|)
  reconstruct' :: Finger bst a -> bst a
  reconstruct' f@(_, _, []) = unPoint' f
  reconstruct' f            = parent' f >- reconstruct'

  -- moving the finger around, fail if can't move
  -- W/S - O(1)
  parent', childL', childR':: Finger bst a -> Finger bst a
  parent' (t, (lo, Finite k), L r hi' : ps) =
    (Node k (unexpose t, r), (lo, hi'), ps)
  parent' (t, (Finite k, hi), R l lo' : ps) =
    (Node k (l, unexpose t), (lo', hi), ps)
  parent' _ = error "root"
  childL' (t, (lo, hi), ps) = case t of
    Leaf          -> error "leaf"
    Node k (l, r) -> (expose l, (lo, Finite k), L r hi : ps)
  childR' (t, (lo, hi), ps) = case t of
    Leaf          -> error "leaf"
    Node k (l, r) -> (expose r, (Finite k, hi), R l lo : ps)

  -- bulk moves, fail if tree is empty
  -- root: W/S - O(1)
  -- minimal, maximal: W/S - (lg |t|)
  root', minimal', maximal' :: Finger bst a -> Finger bst a
  root' f@(Node {}, _, []) = f
  root' f = parent' f >- root'
  minimal' f@(_, (NegInf, _), _) = inf' f
  minimal' f = parent' f >- minimal'
  maximal' f@(_, (_, PosInf), _) = sup' f
  maximal' f = parent' f >- maximal'
  -- minimal/maximal in subtree, unless Leaf node, in which case return parent
  inf', sup' :: Finger bst a -> Finger bst a
  inf' f@(Leaf, _, _) = parent' f
  inf' f = childL' f >- inf'
  sup' f@(Leaf, _, _) = parent' f
  sup' f = childR' f >- sup'

  -- rotations, fail if either of two nodes are Leafs
  -- W/S - O(1)
  rotateL', rotateR' :: Finger bst a -> Finger bst a
  rotateL' (Leaf, _, _) = error "leaf"
  rotateL' (Node k (l, r), i, ps) =
    case expose r of
      Leaf -> error "right leaf"
      Node rk (rl, rr) -> (Node rk (l', rr), i, ps)
        where l' = unexpose $ Node k (l, rl)
  rotateR' (Leaf, _, _) = error "leaf"
  rotateR' (Node k (l, r), i, ps) =
    case expose l of
      Leaf -> error "left leaf"
      Node lk (ll, lr) -> (Node lk (ll, r'), i, ps)
        where r' = unexpose $ Node k (lr, r)

  -- iterating using fingers, fail if maximal/minimal Leaf/Node
  -- W/S - O(1) expected
  prev', next' :: Ord a => Finger bst a -> Finger bst a
  prev' f@(Leaf, _, _) = parentL' f
  prev' f@(Node k (l, _), _, _) = case expose l of
    Leaf -> parentL' f
    _ -> sup' . childL' $ f
  next' f@(Leaf, _, _) = parentR' f
  next' f@(Node k (_, r), _, _) = case expose r of
    Leaf -> parentR' f
    _ -> inf' . childR' $ f
  parentL', parentR' :: Finger bst a -> Finger bst a
  parentL' f@(_, (_, Finite _), p : _) = case p of
    L {} -> parent' f
    R {} -> parent' f >- parentL'
  parentL' _ = error "no left parent"
  parentR' f@(_, (Finite _, _), p : _) = case p of
    L {} -> parent' f >- parentL'
    R {} -> parent' f
  parentR' _ = error "no right parent"

  -- searching a tree, returns Leaf finger if not in tree
  -- W/S - O(lg |t|) expected
  search' :: Ord a => a -> bst a -> Finger bst a
  search' k = point' >-> searchFrom' k
  -- searching from a finger
  -- W/S - O(lg(j - i + 1)) expected
  -- where i, j are ranks of the start and end keys
  searchFrom' :: Ord a => a -> Finger bst a -> Finger bst a
  searchFrom' k = sf' where
    sf' f@(tv, i, _) = case tv of
      Leaf -> if inRange k i then f else parent' f >- sf'
      Node k' _ -> if inRange k i
        then case compare k k' of
          EQ -> f
          LT -> childL' f >- sf'
          GT -> childR' f >- sf'
        else parent' f >- sf'

  -- last non-leaf node accessed, unless tree is empty
  -- W/S - O(1)
  lastAccessed' :: Finger bst a -> Finger bst a
  lastAccessed' f@(Leaf, _, _) = parent' f
  lastAccessed' f = f
