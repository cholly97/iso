module Splay where

import           Finger
import           SelfBalancing
import           Trees
import           Utils

import           Control.Lens
import           Data.Maybe

data SplayTree a = E | N a (SplayTree a, SplayTree a)

instance BST SplayTree where
  expose E            = Leaf
  expose (N k (l, r)) = Node k (l, r)
  unexpose (Node k (l, r)) = N k (l, r)
  unexpose Leaf            = E
  empty = E
  singleton k = N k (E, E)

instance FingerBST SplayTree

instance Show a => Show (SplayTree a) where
  show = toString2d 1

-- W/S - O(1)
splayOp :: Finger SplayTree a -> Maybe (Finger SplayTree a)
splayOp f@(_, _, ps) = case ps of
  L{} : L{} : _ -> parent f >>= parent >>= rotateR >>= rotateR
  R{} : R{} : _ -> parent f >>= parent >>= rotateL >>= rotateL
  L{} : R{} : _ -> parent f >>= rotateR >>= parent >>= rotateL
  R{} : L{} : _ -> parent f >>= rotateL >>= parent >>= rotateR
  L{}       : _ -> parent f >>= rotateR
  R{}       : _ -> parent f >>= rotateL
  []            -> Nothing
-- W/S - O(lg |t|) expected
splay :: Finger SplayTree a -> Finger SplayTree a
splay = doUntilNothing splayOp

instance SelfBalancing SplayTree where
  join t1 t2 = maybe t2 join' $ sup t1
   where
    join'    = splay >-> replaceR >-> reconstruct
    replaceR = set >- _1 . Trees.children . _2 >- t2
    -- partial function because splaying largest element to root
    -- should always result in empty right child, and no parent
    -- replaceR (Node k (l, empty), (NegInf, PosInf), []) =
      -- (Node k (l, t2), (NegInf, PosInf), [])
    -- replaceR _ = error "bad implementation of splay and/or sup"

  split k t = case splay . lastAccessed $ search' k t of
    (Leaf          , _, _) -> (False, empty, empty)
    (Node k' (l, r), _, _) -> case compare k k' of
      EQ -> (True, l, r)
      LT -> (False, l, unexpose $ Node k (empty, r))
      GT -> (False, unexpose $ Node k (l, empty), r)
