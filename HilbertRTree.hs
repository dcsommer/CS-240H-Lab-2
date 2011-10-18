module HilbertRTree(HilbertRTree, insert, search, empty, maxReturn) where

import Rect
import Internal
import Control.Exception

-- Constants
------------

--This is the maximum number of results returned by `search`
maxReturn = 4

-- Inserting into a leaf is not allowed. No leaf should be passed into
-- `insert`. This should be fine because clients never get a handle to
-- a Leaf, only a "None" or an "Interior"
insert :: HilbertRTree -> Rect -> HilbertRTree
-- First, base case for inserting into an empty HRT
insert None rect =
  let toInsert = makeLeafEntry rect in
  Interior [NodeEntry rect (Leaf [toInsert]) (lhv toInsert)]
-- Inserting into a HilbertRTree. The guard lets us know if
-- the implementation is broken internally
insert (Interior entries) rect | (length entries > 0) =
  let toInsert = makeLeafEntry rect in
  -- Inserting into a non-empty interior node
  case (insertI toInsert $ algC3 entries (lhv toInsert)) of
    Return1 old -> Interior old
    Return2 old new -> Interior $ new:[liftInteriorEntries old]

search :: HilbertRTree -> Rect -> [Rect]
search (Leaf children) rect = searchF children rect []
search (Interior children) rect = searchF children rect []
search None _ = []

searchF :: [NodeEntry] -> Rect -> [Rect] -> [Rect]
searchF (e@(NodeEntry{ mbr = x1 }):rest) rect acc =
  if (length acc < maxReturn)
  then if (intersectRect x1 rect)
       then searchF rest rect (searchI e rect acc)
       else searchF rest rect acc
  else acc
    where searchI NodeEntry{ mbr = x2, children = c2 } rect acc = 
            if (length acc < maxReturn) && (intersectRect x2 rect)
            then case c2 of
              Leaf childEntries -> searchF childEntries rect acc
              Interior childEntries -> searchF childEntries rect acc
              None -> x2:acc
            else acc
searchF [] _ acc = acc

makeLeafEntry rect =
  NodeEntry { mbr = rect, children = None, lhv = hilbertValue rect}