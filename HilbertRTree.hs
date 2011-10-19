module HilbertRTree(HilbertRTree, insert, search, empty, maxReturn) where

import Rect
import HilbertRTree.Internal

-- Constants
------------

--This is the maximum number of results returned by `search`
maxReturn :: Int
maxReturn = 4

insert :: HilbertRTree -> Rect -> HilbertRTree
-- First, base case for inserting into an empty HRT
insert None rect =
  let toInsert = makeLeafEntry rect in
  Interior [NodeEntry rect (Leaf [toInsert]) (lhv toInsert)]
-- Inserting into a HilbertRTree. The guard lets us know if
-- the implementation is broken internally
insert (Interior entries) rect 
  | length entries <= 0 = error "client somehow has empty node"
  | otherwise           = let toInsert = makeLeafEntry rect in
  -- Inserting into a non-empty interior node
  case insertI toInsert $ algC3 entries (lhv toInsert) of
    Return1 old -> Interior old
    Return2 old new -> Interior [liftInteriorEntries (new:old)]
-- Inserting into a leaf is not allowed. No leaf should be passed into
-- `insert`. This should be fine because clients never get a handle to
-- a Leaf, only a "None" or an "Interior"
insert (Leaf _) _ = error "client somehow constructed or was given a Leaf"

search :: HilbertRTree -> Rect -> [Rect]
search (Leaf cs) rect = searchF cs rect []
search (Interior cs) rect = searchF cs rect []
search None _ = []

searchF :: [NodeEntry] -> Rect -> [Rect] -> [Rect]
searchF (e@(NodeEntry{ mbr = x1 }):rest) rect1 acc =
  if length acc < maxReturn
  then if intersectRect x1 rect1
       then searchF rest rect1 (searchI e rect1 acc)
       else searchF rest rect1 acc
  else acc
    where searchI NodeEntry{ mbr = x2, children = c2 } rect2 acc2 = 
            if length acc < maxReturn && intersectRect x2 rect2
            then case c2 of
              Leaf childEntries -> searchF childEntries rect2 acc2
              Interior childEntries -> searchF childEntries rect2 acc2
              None -> x2:acc2
            else acc2
searchF [] _ acc = acc

makeLeafEntry :: Rect -> NodeEntry
makeLeafEntry rect =
  NodeEntry { mbr = rect, children = None, lhv = hilbertValue rect}