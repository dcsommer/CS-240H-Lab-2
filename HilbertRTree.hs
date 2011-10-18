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
search (Leaf children) rect = foldl searchF [] children
  searchF soFar NodeEntry{ mbr = x } = 
    if (length soFar < maxReturn) && (intersectRect x rect)
    then x:soFar
    else soFar
search (Interior children) rect = foldl searchF [] children where
  searchF soFar NodeEntry{ mbr = x, children = c } = 
    if (length soFar < maxReturn) && (intersectRect x rect)
    then soFar ++ search c rect
    else soFar
search None _ = []

makeLeafEntry rect =
  NodeEntry { mbr = rect, children = None, lhv = hilbertValue rect}