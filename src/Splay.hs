module Splay where

import           Finger
import           SelfBalancing
import           Trees
import           Utils

import           Control.Lens

data SplayTree a = E | N a (SplayTree a, SplayTree a)

instance BST SplayTree where
  expose E            = Leaf
  expose (N k (l, r)) = Node k (l, r)
  unexpose (Node k (l, r)) = N k (l, r)
  unexpose Leaf            = E
  empty = E
  singleton k = N k (E, E)

instance FingerBST SplayTree
instance FingerBST' SplayTree

instance Show a => Show (SplayTree a) where
  show = toString2d 1

-- W/S - O(1)
splayOp :: Finger SplayTree a -> Maybe (Finger SplayTree a)
splayOp f@(_, _, ps) = f >- case ps of
  L{} : L{} : _ -> parent >>=> parent >>=> rotateR >>=> rotateR
  R{} : R{} : _ -> parent >>=> parent >>=> rotateL >>=> rotateL
  L{} : R{} : _ -> parent >>=> rotateR >>=> parent >>=> rotateL
  R{} : L{} : _ -> parent >>=> rotateL >>=> parent >>=> rotateR
  L{}       : _ -> parent >>=> rotateR
  R{}       : _ -> parent >>=> rotateL
  []            -> const Nothing
splayOp' :: Finger SplayTree a -> Finger SplayTree a
splayOp' f@(_, _, ps) = f >- case ps of
  L{} : L{} : _ -> parent' >-> parent' >-> rotateR' >-> rotateR'
  R{} : R{} : _ -> parent' >-> parent' >-> rotateL' >-> rotateL'
  L{} : R{} : _ -> parent' >-> rotateR' >-> parent' >-> rotateL'
  R{} : L{} : _ -> parent' >-> rotateL' >-> parent' >-> rotateR'
  L{}       : _ -> parent' >-> rotateR'
  R{}       : _ -> parent' >-> rotateL'
  []            -> error "root"
-- W/S - O(lg |t|) expected
splay, splay' :: Finger SplayTree a -> Finger SplayTree a
splay = doUntilNothing splayOp
-- version that uses splayOp'
splay' f@(_, _, []) = f
splay' f            = splayOp' f >- splay'

instance SelfBalancing SplayTree where
  join t1 t2 = case expose t1 of
    Leaf -> t2
    _    -> point' t1 >- maximal' >- joinWitht2
   where
    joinWitht2 = splay' >-> replaceRt2 >-> unexpose
    replaceRt2 = set -< Trees.children . _2 -< t2 <-< lift
    -- partial function because splaying largest element to root
    -- should always result in empty right child, and no parent
    -- replaceR (Node k (l, _), (NegInf, PosInf), []) = Node k (l, t2)
    -- replaceR _ = error "bad implementation of splay and/or sup"

  split k t = case search' k t >- lastAccessed' >- splay' of
    (Leaf          , _, _) -> (False, empty, empty)
    (Node k' (l, r), _, _) -> case compare k k' of
      EQ -> (True, l, r)
      LT -> (False, l, unexpose $ Node k (empty, r))
      GT -> (False, unexpose $ Node k (l, empty), r)
