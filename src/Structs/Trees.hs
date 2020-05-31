{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Structs.Trees where

import           Utils.Combinators
import           Utils.List

import           Control.Lens
import qualified Control.Monad                 as Mon
import           Data.List

data TreeView t a = Leaf | Node {_key::a, _children:: (t a, t a)}
$(makeLenses ''TreeView)

getValue :: TreeView t a -> Maybe a
getValue (Node k _) = Just k
getValue _          = Nothing

instance Eq a => Eq (TreeView t a) where
  t1 == t2 = getValue t1 == getValue t2

instance Ord a => Ord (TreeView t a) where
  compare f1 f2 = compare (getValue f1) (getValue f2)

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
  fexpose :: (bst a -> bst b) -> TreeView bst a -> TreeView bst b
  fexpose f = unexpose >-> f >-> expose
  funexpose :: (TreeView bst a -> TreeView bst b) -> bst a -> bst b
  funexpose f = expose >-> f >-> unexpose

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
              height = max -< length lss -< length rss
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
              padL = take padLenL -< padderL
              padR = take padLenR -< padderR >- reverse
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
              lssPadded = spacing * (2 * l + 1) >- spaces >- rpad height -< lss
              rssPadded = spacing * (2 * r + 1) >- spaces >- rpad height -< rss
              -- w(n) = w(l) + w(r) + spacing
              --      = spacing * (2 * l + 1 + 2 * r + 1 + 1)
              --      = spacing * (2 * (l + r + 1) + 1)
              --      = spacing * (2 * n + 1)
              combine ls rs = mconcat [ls, spaces spacing, rs]
              childrenRep = zipWith combine lssPadded rssPadded
          in (fmap padSpaces rootRep ++ childrenRep, l, r, 1)

instance BST bst => Functor (TreeView bst) where
  fmap f Leaf = Leaf
  fmap f (Node k (l, r)) =
    Node (f k) (funexpose -< fmap f -< l, funexpose -< fmap f -< r)
