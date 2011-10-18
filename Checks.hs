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

-- Insert 2 different rects a1 and a2 times respectively and depending on
-- whether they overlap, checks to make sure the right number of search
-- results are returned
p_insert_many_diff_result :: Rect -> Rect -> Word8 -> Word8 -> Bool
p_insert_many_diff_result r1 r2 a1 a2 =
  let makeSmaller x = fromIntegral $ x `mod` 25
      a1' = makeSmaller a1
      a2' = makeSmaller a2
      result = length $ search (fillRec r1 a1' (insertEmpty r2 a2')) r1
      expected = min maxReturn $ if intersectRect r1 r2
                                 then a1' + a2' else a1' in
  result == expected


--helpers
insertMultiResult :: (Ord a, Num a, Ord b, Num b) => Rect -> a -> b
insertMultiResult rect x =
  fromIntegral $ length $ search (insertEmpty rect x) rect

insertEmpty :: (Ord a, Num a) => Rect -> a -> HilbertRTree
insertEmpty rect x = fillRec rect x empty

fillRec rect x tree | x == 0 = tree
                    | x > 0  = fillRec rect (x-1) (insert tree rect)