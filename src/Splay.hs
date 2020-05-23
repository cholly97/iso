module Splay where

import           Finger
import           SelfBalancing
import           Trees
import           Utils

import           Control.Lens
import           Control.Zipper
import           Data.List.NonEmpty

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
splayOp, splaySemiOp :: Finger SplayTree a -> Maybe (Finger SplayTree a)
splayOp f@(_, _, ps) = f >- case ps of
  L{} : L{} : _ -> parent >>=> parent >>=> rotateR >>=> rotateR
  R{} : R{} : _ -> parent >>=> parent >>=> rotateL >>=> rotateL
  L{} : R{} : _ -> parent >>=> rotateR >>=> parent >>=> rotateL
  R{} : L{} : _ -> parent >>=> rotateL >>=> parent >>=> rotateR
  L{}       : _ -> parent >>=> rotateR
  R{}       : _ -> parent >>=> rotateL
  []            -> const Nothing
splaySemiOp f@(_, _, ps) = f >- case ps of
  L{} : L{} : _ -> parent >>=> parent >>=> rotateR
  R{} : R{} : _ -> parent >>=> parent >>=> rotateL
  _             -> splayOp
splayOp' :: Finger SplayTree a -> Finger SplayTree a
splayOp' f = f >- case Data.List.NonEmpty.fromList $ parents f of
  L{} :| L{} :  _ -> parent' >-> parent' >-> rotateR' >-> rotateR'
  R{} :| R{} :  _ -> parent' >-> parent' >-> rotateL' >-> rotateL'
  L{} :| R{} :  _ -> parent' >-> rotateR' >-> parent' >-> rotateL'
  R{} :| L{} :  _ -> parent' >-> rotateL' >-> parent' >-> rotateR'
  L{}        :| _ -> parent' >-> rotateR'
  R{}        :| _ -> parent' >-> rotateL'
splaySemiOp' f = f >- case Data.List.NonEmpty.fromList $ parents f of
  L{} :| L{} : _ -> parent' >-> parent' >-> rotateR'
  R{} :| R{} : _ -> parent' >-> parent' >-> rotateL'
  _              -> splayOp'
-- W/S - O(lg |t|) expected
splay, splay', splaySemi, splaySemi' :: Finger SplayTree a -> Finger SplayTree a
splay = farthest splayOp
splaySemi = farthest splaySemiOp
-- version that uses splayOp'
splay' f = case parents f of
  [] -> f
  _  -> splayOp' f >- splay'
splaySemi' f = case parents f of
  [] -> f
  _  -> splaySemiOp' f >- splaySemi'

instance SelfBalancing SplayTree where
  join t1 t2 = case expose t1 of
    Leaf -> t2
    _    -> point' t1 >- maximal' >- splay' >- lift >- replaceRt2 >- unexpose
      where replaceRt2 = set -< Trees.children . _2 -< t2
  split k t = case search' k t >- lastAccessed' >- splay' of
    (Leaf          , _, _) -> (False, empty, empty)
    (Node k' (l, r), _, _) -> case compare k k' of
      EQ -> (True, l, r)
      LT -> (False, l, unexpose $ Node k (empty, r))
      GT -> (False, unexpose $ Node k (l, empty), r)

  -- alternate definitions of delete and insert that are slightly more efficient
  -- also works with splaySemiing, unlike the join/split based algorithm, since
  -- semiplaying doesn't neccesarily bring the accessed node to the root
  delete k = search' k >-> over _1 deleteNode >-> splayParent >-> reconstruct'
   where
    deleteNode :: TreeView SplayTree a -> TreeView SplayTree a
    deleteNode Leaf              = Leaf
    deleteNode (Node _ (t1, t2)) = expose $ join t1 t2
    splayParent :: Finger SplayTree a -> Finger SplayTree a
    splayParent = parent >>--> splay' >- tug
  insert k = search' k >-> over _1 insertNode >-> splay' >-> reconstruct'
   where
    -- cannot use the following type signature because using k fixes "a"
    -- insertNode :: TreeView SplayTree a -> TreeView SplayTree a
    insertNode Leaf = Node k (empty, empty)
    insertNode f    = f
