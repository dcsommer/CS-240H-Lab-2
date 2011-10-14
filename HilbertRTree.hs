module HilbertRTree(Rect(Rect), xLow, xHigh, yLow, yHigh,
                    HilbertRTree, insert, search, emptyHRT) where

import Data.Word(Word16)
import Data.Bits
import Data.Lens.Common

-- Constants
------------
word16Max = (-1 :: Word16) --This becomes (2^16 - 1)
maxOverlap = 4     --This is the maximum number of results returned by
                   --  `search`
             
c_l = 4            -- number of entries in a leaf node
c_n = 3            -- number of entries in a non-leaf node

handleOverflow = 2
adjustTree = 3

-- Types
--------

-- The Largest Hilbert Value (LHV) is really a Word16
type LHV = Word16

-- A rect 
data Rect = Rect { xLow :: Word16
                 , xHigh :: Word16
                 , yLow :: Word16
                 , yHigh :: Word16
                 } deriving Show

-- A minimum bounding rectangle (MBR) is a rectangle
type MBR = Rect

------- HilbertRTree

data NodeEntry = NodeEntry { mbr :: MBR
                           , child :: HilbertRTree
                           , lhv :: LHV
                           }

-- Leafs hold at most C_l entries
-- Nodes hold at most C_n entries
data HilbertRTree = Leaf [Rect] | Interior [NodeEntry]

emptyHRT = Leaf []

insert :: HilbertRTree -> Rect -> HilbertRTree
insert (Interior [rs]) r = let h = xy2d ((xLow r) +
                                         div (xHigh r - xLow r) 2) in
  Leaf [r]
  
insert (Leaf val) rect =
  Leaf [rect]

search :: HilbertRTree -> Rect -> [Rect]
search (Leaf val) rect = []
search (Interior children) rect =
  foldl searchF [] children where
    searchF :: [Rect] -> NodeEntry -> [Rect]
    searchF soFar NodeEntry{ mbr = x, child = c } = 
      if (length soFar < maxOverlap) && (intersect' x rect)
      then soFar ++ search c rect
      else soFar
  

------ Private helpers

chooseLeaf rect h (Interior (e:es)) = chooseLeaf rect h $ child e
chooseLeaf rect h (Leaf x) = Leaf x

-- Check to see if these two rectangles intersect
intersect' :: Rect -> Rect -> Bool
intersect' r1 r2 = not (xHigh r1 < xLow r2 ||
                        xLow r1 > xHigh r2 ||
                        yHigh r1 < yLow r2 ||
                        yLow r1 > yHigh r2)
                   
-- Calculate the Hilbert distance for the given point (x,y)
--   Adapted from the iterative version of this algorithm at:
--   http://en.wikipedia.org/wiki/Hilbert_curve 
xy2d :: Word16 -> Word16 -> Word16
xy2d x y = xy2dRec (2^15::Word16) (x,y) 0 where
    xy2dRec 0 (x,y) d = d
    xy2dRec s (x,y) d = xy2dRec (div s 2) rotated (d + step) where
           rx = if (x .&. s) > 0 then 1 else 0
           ry = if (y .&. s) > 0 then 1 else 0
           rotated = rot s (x,y) rx ry
           step = s * s * ((3 * rx) ^ ry)

-- Rotate and/or flip a quadrent appropriately
rot s (x,y) rx ry =
    if ry == 0
    then if rx == 1
         then (s - 1 - y, s - 1 - x)
         else (y,x)
    else (x,y)