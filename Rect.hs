module Rect(Rect(Rect), intersectRect, hilbertValue, boundRects, LHV) where

import Data.Word(Word16,Word32)
import Data.Bits


-- A 'Largest Hilbert Value' (LHV) is really a Word32. For a space
-- ([0,n-1, [0,n-1]), the hilbert value can be in [0, n^2 - 1], so we
-- need 32 bits here
type LHV = Word32

-- A rect 
data Rect = Rect { xLow :: Word16
                 , xHigh :: Word16
                 , yLow :: Word16
                 , yHigh :: Word16
                 } deriving Show
                            
instance Eq Rect where
         (==) a b = (xLow a) == (xLow b) &&
                    (xHigh a) == (xHigh b) &&
                    (yLow a) == (yLow b) &&
                    (yHigh a) == (yHigh b)


-- Check to see if these two rectangles intersect
intersectRect :: Rect -> Rect -> Bool
intersectRect r1 r2 = not (xHigh r1 < xLow r2 ||
                           xLow r1 > xHigh r2 ||
                           yHigh r1 < yLow r2 ||
                           yLow r1 > yHigh r2)
                   
-- Calculate the Hilbert value of a rectangle via it's center
-- Note the implementation of average here avoids overflow
hilbertValue :: Rect -> LHV
hilbertValue r = xy2d ((xLow r) + div (xHigh r - xLow r) 2)
                      ((yLow r) + div (yHigh r - yLow r) 2)

-- Get the MBR for two rectangles
boundRects :: Rect -> Rect -> Rect
boundRects a@Rect{} b@Rect{} =
  Rect { xLow = min (xLow a) (xLow b)
       , xHigh = max (xHigh a) (xHigh b)
       , yLow = min (yLow a) (yLow b)
       , yHigh = max (yHigh a) (yHigh b) }

-- Calculate the Hilbert distance for the given point (x,y)
--   Adapted from the iterative version of this algorithm at:
--   http://en.wikipedia.org/wiki/Hilbert_curve 
xy2d :: Word16 -> Word16 -> LHV
xy2d x y = xy2dRec (2^15::Word16) (x,y) 0 where
    xy2dRec 0 (x,y) d = d
    xy2dRec s (x,y) d = xy2dRec (div s 2) rotated (d + step) where
           s32 = fromIntegral s
           rx = if (x .&. s) > 0 then 1 else 0
           ry = if (y .&. s) > 0 then 1 else 0
           rotated = rot s (x,y) rx ry
           step = s32 * s32 * ((3 * fromIntegral rx) ^ fromIntegral ry)

-- Rotate and/or flip a quadrent appropriately
rot s (x,y) rx ry =
    if ry == 0
    then if rx == 1
         then (s - 1 - y, s - 1 - x)
         else (y,x)
    else (x,y)
