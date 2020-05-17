-- {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SelfBalancing where

import           Trees
import           Utils

class BST bst => SelfBalancing bst where
  -- each function hereafter
  -- requires input ts are sorted
  -- ensures output ts are sorted

  fromList :: Ord a => [a] -> bst a
  fromList = foldl -< flip insert -< empty

  -- parametric axioms
  -- join t1 t2 = t
  -- requires t1 < t2
  -- ensures inord t1 ++ inord t2 == inord t
  -- W/S - O(lg (|t1| + |t2|))
  join :: bst a -> bst a -> bst a
  -- split t x = (b, l, r)
  -- ensures b = x in t, l < x < r
  -- W/S - O(lg |t|)
  split, split' :: Ord a => a -> bst a -> (Bool, bst a, bst a)
  -- can use split = split' but native implementation may be desired
  split' k t = case expose t of -- split implemented using joinM
    Leaf -> (False, empty, empty)
    Node k' (l, r) -> case compare k k' of
      EQ -> (True, l, r)
      LT -> let (b, l', r') = split k l in (b, l', joinM k' r' r)
      GT -> let (b, l', r') = split k r in (b, joinM k' l l', r')
  -- alternate form of join
  -- joinM k t1 t2 = t
  -- requires t1 < k < t2
  -- ensures inord t1 ++ [k] ++ inord t2 == inord t
  -- W/S - O(1)
  joinM, joinM' :: a -> bst a -> bst a -> bst a
  joinM  x t1 t2 = unexpose $ Node x (t1, t2)
  -- same as joinM except W/S - O(lg (|t1| + |t2|))
  joinM' x t1 t2 = join t1 -< singleton x >- flip join t2

  -- derived basic functions
  -- W/S - O(lg |t|)
  find :: Ord a => a -> bst a -> Bool
  find k t =
    let (b, _, _) = split k t
    in b
  delete, insert :: Ord a => a -> bst a -> bst a
  delete k t =
    let (_, l, r) = split k t
    in join l r
  insert k t =
    let (_, l, r) = split k t
    in joinM k l r

  -- derived batch functions
  -- let m = min(|t1|, |t2|), n = max(|t1|, |t2|)
  -- W - O(m * lg ((n + m)/m))
  -- S - O(lg n)
  intersect, difference, union :: Ord a => bst a -> bst a -> bst a
  intersect t1 t2 = case (expose t1, expose t2) of
        (Leaf, _) -> empty
        (_, Leaf) -> empty
        (Node k1 (l1, r1), _) ->
          let (b, l2, r2) = split k1 t2
              l = intersect l1 l2
              r = intersect r1 r2
          in if b then joinM k1 l r else join l r
  difference t1 t2 = case (expose t1, expose t2) of
    (Leaf, _) -> empty
    (_, Leaf) -> t1
    (Node k1 (l1, r1), _) ->
      let (b, l2, r2) = split k1 t2
          l = difference l1 l2
          r = difference r1 r2
      in if b then join l r else joinM k1 l r
  union t1 t2 = case (expose t1, expose t2) of
    (Leaf, _) -> t2
    (_, Leaf) -> t1
    (Node k1 (l1, r1), _) ->
      let (_, l2, r2) = split k1 t2
          l = union l1 l2
          r = union r1 r2
      in joinM k1 l r
