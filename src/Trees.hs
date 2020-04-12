{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Trees where

import           Utils

import           Control.Lens
import qualified Control.Monad                 as Mon
import           Data.List
import           Data.Maybe

data TreeView t a = Leaf | Node {_key::a, _children:: (t a, t a)}
$(makeLenses ''TreeView)

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
      Node k c -> combine [k] $ Mon.join bimap traversal c
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
  inf' = doUntilNothing childL >-> parent >-> tryMaybe "empty tree" id
  sup' = doUntilNothing childR >-> parent >-> tryMaybe "empty tree" id
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

  -- ((((4)8(29))57((67)88))100((146)210((234)267(317))))
  toString1d :: Show a => bst a -> String
  toString1d t = case expose t of
    Leaf          -> ""
    Node k (l, r) -> mconcat ["(", toString1d l, show k, toString1d r, ")"]

  --         ___100_
  --        /       \
  --     _57__      210_
  --    /     \     /   \
  --   8      88  146   267
  --  / \     /         / \
  -- 4  29  67        234 317
  toString2d :: Show a => Int -> bst a -> String
  toString2d spacing = toString2d' >-> view _1 >-> intersperse "\n" >-> mconcat
    where
      -- each node is 2 * spacing apart from the next largest
      -- bottommost nodes are at least 2 ranks apart
      -- want bottommost nodes to be separated by at least spacing spaces
      -- thus each node has width = 2 * (2 * spacing) - spacing = 3 * spacing
      spaces :: Int -> String
      spaces = flip replicate ' '
      -- returns (s, l, r) where
      -- s is a list of lines of the representation
      -- l is the size of the left subtree
      -- r is the size of the right subtree
      toString2d' :: (BST bst, Show a) => bst a -> ([String], Int, Int, Int)
      toString2d' t = case expose t of
        Leaf -> ([], 0, 0, 0)
        Node k c ->
          let ((lss, ll, lr, li), (rss, rl, rr, ri)) =
                Mon.join bimap toString2d' c
              -- let n be the size of the tree t
              -- let w(n) be the width of the representation of said tree
              -- ni = if expose t == Leaf then 0 else 1
              -- n = l + r + ni
              -- w(n) = spacing * (2 * n + 1)
              l = ll + lr + li
              r = rl + rr + ri
              height = max >- length lss >- length rss
              -- w(n) = padding + rootLen
              --      = spacing * (2 * (ll + li + rr + ri + lr + rl) + 3)
              --      = spacing * (2 * (l + r + 1) + 1)
              --      = spacing * (2 * n + 1)
              rootLen = spacing * (2 * (lr + rl) + 3)
              rep = take rootLen $ show k
              repLen = length rep
              extend = repLen - spacing
              extendL = div extend 2
              extendR = extend - extendL
              padLenL = spacing * (2 * lr + 1) - extendL
              padLenR = spacing * (2 * rl + 1) - extendR
              -- rootLen = repLen + padLenL + padLenR
              --         = extend + spacing +
              --           spacing * (2 * lr + 1) - extendL +
              --           spacing * (2 * rl + 1) - extendR
              --         = spacing * (2 * lr + 1 + 2 * rl + 1) + spacing +
              --           (extend - extendL - extendR)
              --         = spacing * (2 * (lr + rl) + 3)
              (padderL, branchL) = if null lss
                then (repeat ' ',     " ")
                else (' ':repeat '_', "/")
              (padderR, branchR) = if null rss
                then (repeat ' ',     " ")
                else (' ':repeat '_', "\\")
              padL = take padLenL >- padderL
              padR = take padLenR >- padderR -< reverse
              rootRep = mconcat [padL, rep, padR]:[mconcat [
                branchL,
                spaces $ rootLen - 2,
                branchR
                ] | height > 0]
              padSpaces :: String -> String
              padSpaces line = mconcat [
                spaces $ spacing * 2 * (ll + li),
                line,
                spaces $ spacing * 2 * (rr + ri)]
              lssPadded = spacing * (2 * l + 1) -< spaces -< rpad height >- lss
              rssPadded = spacing * (2 * r + 1) -< spaces -< rpad height >- rss
              -- w(n) = w(l) + w(r) + spacing
              --      = spacing * (2 * l + 1 + 2 * r + 1 + 1)
              --      = spacing * (2 * (l + r + 1) + 1)
              --      = spacing * (2 * n + 1)
              combine ls rs = mconcat [ls, spaces spacing, rs]
              childrenRep = zipWith combine lssPadded rssPadded
          in (fmap padSpaces rootRep ++ childrenRep, l, r, 1)
