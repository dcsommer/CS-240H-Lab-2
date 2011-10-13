module Checks where

import Data.Word(Word16)
import HilbertRTree
import Test.QuickCheck
import Control.Monad
{-
instance Arbitrary (HilbertRTree a) where
    arbitrary = do
      x <- arbitrary
      return (Leaf x)

instance Arbitrary (Rect a) where
    arbitrary = do
      x <- arbitrary
      return (Rect x x x x)

p_insert_there x = search (insert (arbitrary :: HilbertRTree Word16) x)
                          x == []
-}