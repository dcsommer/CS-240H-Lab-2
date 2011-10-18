module Checks where

import Rect
import HilbertRTree
import Internal
import Data.Word(Word16)
import Data.Word(Word8)
import Test.QuickCheck
import Control.Monad

instance Arbitrary Rect where
  arbitrary = do
    r1 <- arbitrary
    r2 <- arbitrary
    r3 <- arbitrary
    r4 <- arbitrary
    return $ Rect (min r1 r2) (max r1 r2) (min r3 r4) (max r3 r4)
    
---- Rect properties
p_overlap_self rect = rect `intersectRect` rect == True
p_bound_self rect = boundRects rect rect == rect
p_bound_assoc r1 r2 r3 = boundRects (boundRects r1 r2) r3 ==
                         boundRects r1 (boundRects r2 r3)

---- HilbertRTree properties
p_insert_in_empty rect = (insertMultiResult rect 1) == 1
  
-- Inserting a rectangle x times into an empty tree should yield x
-- rectangles when searching for that rectangle as long as x <= maxReturn 
p_search_limit :: Rect -> Word8 -> Bool
p_search_limit rect x
  | x <= fromIntegral maxReturn = result == x
  | otherwise                   = result == fromIntegral maxReturn
    where result = (insertMultiResult rect x)
  
-- Inserting 2 non overlapping rects and searching for one should yield
-- one, but if they overlap, two should be returned
          
p_insert_diff_result :: Rect -> Rect -> Bool
p_insert_diff_result r1 r2 =
    if intersectRect r1 r2 then result == 2 else result == 1
      where result = length $ search (insert (insert empty r1) r2) r1

p_insert_diff_with_noise_result :: Rect -> Word8 -> Rect -> Rect -> Bool
p_insert_diff_with_noise_result nonce x r1 r2 = True


--helpers
insertMultiResult :: (Ord a, Num a, Ord b, Num b) => Rect -> a -> b
insertMultiResult rect x =
  fromIntegral $ length $ search (insertEmpty rect x) rect

insertEmpty :: (Ord a, Num a) => Rect -> a -> HilbertRTree
insertEmpty rect x = fillRec x empty
  where fillRec x tree | x == 0 = tree
                       | x > 0  = fillRec (x-1) (insert tree rect)