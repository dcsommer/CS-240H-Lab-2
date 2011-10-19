module Rect(Rect(Rect), intersectRect, hilbertValue, boundRects, LHV) where

import Data.Word(Word16,Word32)
import Data.Bits
import Text.Printf
import Test.QuickCheck hiding ((.&.))

-- A 'Largest Hilbert Value' (LHV) is really a Word32. For a space
-- ([0,n-1, [0,n-1]), the hilbert value can be in [0, n^2 - 1], so we
-- need 32 bits here
type LHV = Word32

-- A rect 
data Rect = Rect { xLow :: Word16
                 , xHigh :: Word16
                 , yLow :: Word16
                 , yHigh :: Word16
                 }

instance Arbitrary Rect where
  arbitrary = do
    r1 <- arbitrary
    r2 <- arbitrary
    r3 <- arbitrary
    r4 <- arbitrary
    return $ Rect (min r1 r2) (max r1 r2) (min r3 r4) (max r3 r4)                            
instance Show Rect where
  show Rect { xLow = x1, xHigh = x2, yLow = y1, yHigh = y2 } =
    printf "(Rect %d %d %d %d)" x1 x2 y1 y2

instance Eq Rect where
         (==) a b = xLow a  == xLow b  &&
                    xHigh a == xHigh b &&
                    yLow a  == yLow b  &&
                    yHigh a == yHigh b

-- Check to see if these two rectangles intersect
intersectRect :: Rect -> Rect -> Bool
intersectRect r1 r2 = not (xHigh r1 < xLow r2 ||
                           xLow r1 > xHigh r2 ||
                           yHigh r1 < yLow r2 ||
                           yLow r1 > yHigh r2)
                   
-- Calculate the Hilbert value of a rectangle via it's center
-- Note the implementation of average here avoids overflow
hilbertValue :: Rect -> LHV
hilbertValue r = xy2d (xLow r + div (xHigh r - xLow r) 2)
                      (yLow r + div (yHigh r - yLow r) 2)

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
xy2d x y = xy2dRec ((2::Word16)^(15::Word16)) (x,y) 0 where
    xy2dRec :: Word16 -> (Word16, Word16) -> Word32 -> Word32
    xy2dRec 0 (_,_) d = d
    xy2dRec s (x', y') d = xy2dRec (div s 2) rotated (d + step) where
           s32 = fromIntegral s :: Word32
           rx = if (x' .&. s) > 0 then 1 else 0 :: Word16
           ry = if (y' .&. s) > 0 then 1 else 0 :: Word16
           rotated = rot s (x', y') rx ry
           base = 3 * fromIntegral rx :: Word32
           power = fromIntegral ry :: Word32
           step = s32 * s32 * (base ^ power)

-- Rotate and/or flip a quadrent appropriately
rot :: Word16 -> (Word16, Word16) -> Word16 -> Word16 -> (Word16, Word16)
rot s (x,y) rx ry =
    if ry == 0
    then if rx == 1
         then (s - 1 - y, s - 1 - x)
         else (y,x)
    else (x,y)
