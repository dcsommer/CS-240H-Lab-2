module HilbertRTree(Rect, xlow, xhigh, ylow, yhigh,
                        HilbertRTree, Leaf, insert, search) where

import Data.Word(Word16)
import Data.Bits

-- Constants
------------
n = (-1 :: Word16) --This becomes (2^16 - 1)
maxOverlap = 4     --This is the maximum number of results returned by
                   --  `search`
c_l = 4            -- number of entries in a leaf node
c_n = 3            -- number of entries in a non-leaf node

-- Types
--------

-- The Largest Hilbert Value (LHV) is really a Word16
type LHV Word16

-- A rect 
data Rect = Rect { xlow :: Word16
                 , xhigh :: Word16
                 , ylow :: Word16
                 , yhigh :: Word16
                 } deriving Show

-- A minimum bounding rectangle (MBR) is a rectangle
type MBR Rect

------- HilbertRTree

-- Leafs hold at most C_l entries
-- Nodes hold at most C_n entries
data HilbertRTree = Leaf [Rect] | Interior MBR [HilbertRTree] LHV

insert :: HilbertRTree -> Rect -> HilbertRTree
insert (Leaf val) (Rect x1 x2 x3 x4) =
    Leaf x1

search :: HilbertRTree -> Rect -> [Rect]
search (Leaf val) (Rect x1 x2 x3 x4) = []
search (Interior val l r) (Rect x1 x2 x3 x4) = []

------ Private helpers

chooseLeaf rect h (Interior a l r) = chooseLeaf rect h l
chooseLeaf rect h (Leaf x) = Leaf x

handleOverflow = 2

adjustTree = 3

-- Calculate the Hilbert distance for the given point (x,y)
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