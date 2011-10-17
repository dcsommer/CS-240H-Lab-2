module Checks where

import Rect
import HilbertRTree
import Data.Word(Word16)
import Test.QuickCheck
import Control.Monad

instance Arbitrary Rect where
  arbitrary = do
    r1 <- arbitrary
    r2 <- arbitrary
    r3 <- arbitrary
    r4 <- arbitrary
    return $ Rect (min r1 r2) (max r1 r2) (min r3 r4) (max r3 r4)
    
-- Rect properties
p_overlap_self rect = rect `intersectRect` rect == True
p_bound_self rect = boundRects rect rect == rect
p_bound_assoc r1 r2 r3 = boundRects (boundRects r1 r2) r3 ==
                         boundRects r1 (boundRects r2 r3)
-- HilbertRTree properties
p_insert_in_empty rect =
  (length (search (insert empty rect) rect)) == 1