module Structs.Finger where

import           Structs.Trees
import           Utils.Combinators
import           Utils.Function
import           Utils.Maybe

import           Control.Monad
import           Control.Zipper

maxInterval :: Interval a
maxInterval = (Nothing, Nothing)

inRange :: Ord a => Interval a -> a -> Bool
inRange (lo, hi) k = lo < Just k && Just k <! hi

type Interval a = (Maybe a, Maybe a)
data Parent t a = L (t a) (Maybe a) | R (t a) (Maybe a)
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
  reconstruct = unexpose . lift >-> flip maybe reconstruct >- ap -< parent

  -- moving the finger around, fail if can't move
  -- W/S - O(1)

  parent, childL, childR :: Finger bst a -> Maybe (Finger bst a)
  parent (t, (lo, Just k), L r hi' : ps) =
    Just (Node k (unexpose t, r), (lo, hi'), ps)
  parent (t, (Just k, hi), R l lo' : ps) =
    Just (Node k (l, unexpose t), (lo', hi), ps)
  parent _ = Nothing
  childL (t, (lo, hi), ps) = case t of
    Leaf          -> Nothing
    Node k (l, r) -> Just (expose l, (lo, Just k), L r hi : ps)
  childR (t, (lo, hi), ps) = case t of
    Leaf          -> Nothing
    Node k (l, r) -> Just (expose r, (Just k, hi), R l lo : ps)

  -- bulk moves, fail if tree is empty
  -- root: W/S - O(1)
  -- minimal, maximal: W/S - (lg |t|)

  root, minimal, maximal :: Finger bst a -> Maybe (Finger bst a)
  root f = f >- case f of
    (Node {}, _, []) -> Just
    _                -> parent >>=> root
  minimal f = f >- case interval f of
    (Nothing, _) -> inf
    _            -> parent >>=> minimal
  maximal f = f >- case interval f of
    (_, Nothing) -> sup
    _            -> parent >>=> maximal
  -- minimal/maximal in subtree, unless Leaf node, in which case return parent

  inf, sup :: Finger bst a -> Maybe (Finger bst a)
  inf = farthest childL >-> parent
  sup = farthest childR >-> parent

  -- rotations, fail if either of two nodes are Leafs
  -- W/S - O(1)

  rotateR, rotateL :: Finger bst a -> Maybe (Finger bst a)
  rotateR (Leaf, _, _) = Nothing
  rotateR (Node k (l, r), i, ps) = case expose l of
    Leaf             -> Nothing
    Node lk (ll, lr) -> Just (Node lk (ll, r'), i, ps)
      where r' = unexpose $ Node k (lr, r)
  rotateL (Leaf, _, _) = Nothing
  rotateL (Node k (l, r), i, ps) = case expose r of
    Leaf             -> Nothing
    Node rk (rl, rr) -> Just (Node rk (l', rr), i, ps)
      where l' = unexpose $ Node k (l, rl)

  -- iterating using fingers, fail if doesn't exist
  -- W/S - O(1) expected

  prev, next :: Ord a => Finger bst a -> Maybe (Finger bst a)
  prev f = f >- case lift f of
    Leaf          -> parentL
    Node k (l, _) -> case expose l of
      Leaf        -> parentL
      _           -> childL >>=> sup
  next f = f >- case lift f of
    Leaf          -> parentR
    Node k (_, r) -> case expose r of
      Leaf        -> parentR
      _           -> childR >>=> inf
  parentL, parentR :: Finger bst a -> Maybe (Finger bst a)
  parentL f = f >- case f of
    (_, (Just _, _), p : _) -> case p of
      L {}                    -> parent
      R {}                    -> parent >>=> parentL
    _                         -> const Nothing
  parentR f = f >- case f of
    (_, (_, Just _), p : _) -> case p of
      L {}                    -> parent >>=> parentR
      R {}                    -> parent
    _                         -> const Nothing

  -- searching a tree, returns Leaf finger if not in tree
  -- W/S - O(lg |t|) expected

  search :: Ord a => a -> bst a -> Maybe (Finger bst a)
  search k = point >-> searchFrom k
  -- searching from a finger
  -- W/S - O(lg(j - i + 1)) expected
  -- where i, j are ranks of the start and end keys

  searchFrom :: Ord a => a -> Finger bst a -> Maybe (Finger bst a)
  searchFrom = searchStep >-> recurse

  searchStep :: Ord a => a -> Finger bst a -> Recursive (Finger bst a) (Maybe (Finger bst a))
  searchStep k f@(tv, i, _) = f >- case tv of
    Leaf      -> if inRange i k
      then       Just >-> Ret
      else       parent >-> maybeRec
    Node k' _ -> if inRange i k
      then       case compare k k' of
        EQ    -> Just >-> Ret
        LT    -> childL >-> maybeRec
        GT    -> childR >-> maybeRec
      else       parent >-> maybeRec

  -- last non-leaf node accessed, unless tree is empty
  -- W/S - O(1)

  lastAccessed :: Finger bst a -> Maybe (Finger bst a)
  lastAccessed f = f >- case f of
    (Leaf, _, _) -> parent
    _            -> Just

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
  reconstruct' f = f >- case f of
    (_, _, []) -> unPoint'
    _          -> parent' >-> reconstruct'

  -- moving the finger around, fail if can't move
  -- W/S - O(1)

  parent', childL', childR':: Finger bst a -> Finger bst a
  parent' (t, (lo, Just k), L r hi' : ps) =
    (Node k (unexpose t, r), (lo, hi'), ps)
  parent' (t, (Just k, hi), R l lo' : ps) =
    (Node k (l, unexpose t), (lo', hi), ps)
  parent' _ = error "root"
  childL' (t, (lo, hi), ps) = case t of
    Leaf          -> error "leaf"
    Node k (l, r) -> (expose l, (lo, Just k), L r hi : ps)
  childR' (t, (lo, hi), ps) = case t of
    Leaf          -> error "leaf"
    Node k (l, r) -> (expose r, (Just k, hi), R l lo : ps)

  -- bulk moves, fail if tree is empty
  -- root: W/S - O(1)
  -- minimal, maximal: W/S - (lg |t|)

  root', minimal', maximal' :: Finger bst a -> Finger bst a
  root' f = f >- case f of
    (Node {}, _, []) -> id
    _                -> parent' >-> root'
  minimal' f = f >- case interval f of
    (Nothing, _) -> inf'
    _            -> parent' >-> minimal'
  maximal' f = f >- case interval f of
    (_, Nothing) -> sup'
    _            -> parent' >-> maximal'
  -- minimal/maximal in subtree, unless Leaf node, in which case return parent

  inf', sup' :: Finger bst a -> Finger bst a
  inf' f = f >- case lift f of
    Leaf -> parent'
    _    -> childL' >-> inf'
  sup' f = f >- case lift f of
    Leaf -> parent'
    _    -> childR' >-> sup'

  -- rotations, fail if either of two nodes are Leafs
  -- W/S - O(1)

  rotateL', rotateR' :: Finger bst a -> Finger bst a
  rotateL' (Leaf, _, _) = error "leaf"
  rotateL' (Node k (l, r), i, ps) = case expose r of
    Leaf             -> error "right leaf"
    Node rk (rl, rr) -> (Node rk (l', rr), i, ps)
      where l' = unexpose $ Node k (l, rl)
  rotateR' (Leaf, _, _) = error "leaf"
  rotateR' (Node k (l, r), i, ps) = case expose l of
    Leaf             -> error "left leaf"
    Node lk (ll, lr) -> (Node lk (ll, r'), i, ps)
      where r' = unexpose $ Node k (lr, r)

  -- iterating using fingers, fail if maximal/minimal Leaf/Node
  -- W/S - O(1) expected

  prev', next' :: Ord a => Finger bst a -> Finger bst a
  prev' f = f >- case lift f of
    Leaf          -> parentL'
    Node k (l, _) -> case expose l of
      Leaf        -> parentL'
      _           -> childL' >-> sup'
  next' f = f >- case lift f of
    Leaf          -> parentR'
    Node k (_, r) -> case expose r of
      Leaf        -> parentR'
      _           -> childR' >-> inf'
  parentL', parentR' :: Finger bst a -> Finger bst a
  parentL' f = f >- case f of
    (_, (Just _, _), p : _) -> case p of
      L {}                    -> parent'
      R {}                    -> parent' >-> parentL'
    _                         -> error "no left parent"
  parentR' f = f >- case f of
    (_, (_, Just _), p : _) -> case p of
      L {}                    -> parent' >-> parentR'
      R {}                    -> parent'
    _                         -> error "no right parent"

  -- searching a tree, returns Leaf finger if not in tree
  -- W/S - O(lg |t|) expected

  search' :: Ord a => a -> bst a -> Finger bst a
  search' k = point' >-> searchFrom' k
  -- searching from a finger
  -- W/S - O(lg(j - i + 1)) expected
  -- where i, j are ranks of the start and end keys

  searchFrom' :: Ord a => a -> Finger bst a -> Finger bst a
  searchFrom' = searchStep' >-> recurse

  searchStep' :: Ord a => a -> Finger bst a -> Recursive (Finger bst a) (Finger bst a)
  searchStep' k f@(tv, i, _) = f >- case tv of
    Leaf      -> if inRange i k
      then       Ret
      else       parent' >-> Rec
    Node k' _ -> if inRange i k
      then       case compare k k' of
        EQ    -> Ret
        LT    -> childL' >-> Rec
        GT    -> childR' >-> Rec
      else       parent' >-> Rec

  -- last non-leaf node accessed, unless tree is empty
  -- W/S - O(1)

  lastAccessed' :: Finger bst a -> Finger bst a
  lastAccessed' f = f >- case lift f of
    Leaf -> parent'
    _    -> id
