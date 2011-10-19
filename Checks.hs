import Rect
import HilbertRTree
import Data.Word(Word8)
import Test.QuickCheck.Test
import Internal

---- Rect properties
p_overlap_self :: Rect -> Bool
p_overlap_self rect = rect `intersectRect` rect
p_bound_self :: Rect -> Bool
p_bound_self rect = boundRects rect rect == rect
p_bound_assoc :: Rect -> Rect -> Rect -> Bool
p_bound_assoc r1 r2 r3 = boundRects (boundRects r1 r2) r3 ==
                         boundRects r1 (boundRects r2 r3)

---- HilbertRTree properties
p_insert_in_empty :: Rect -> Bool
p_insert_in_empty rect = insertMultiResult rect (1::Integer) ==
                         (1::Integer) 
  
-- Inserting a rectangle x times into an empty tree should yield x
-- rectangles when searching for that rectangle as long as x <= maxReturn 
p_search_limit :: Rect -> Word8 -> Bool
p_search_limit rect x
  | x <= fromIntegral maxReturn = result == x
  | otherwise                   = result == fromIntegral maxReturn
    where result = insertMultiResult rect x
  
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

-- The layout of internal nodes should be such that the type of the
-- "children" field of each node should follow this ordering:
-- Interior < Leaf < None. Also, the children of a Leaf node MUST 
-- be of None type, the children of an Interior node MUST NOT be
-- of None type. The constructed types of all siblings MUST match.
-- Example valid tree:
--
--                        _________Interior____________
--                       /                             \
--              _____Interior_______________         Interior__
--             /                            \                  \
--       ___Interior__                 __Interior__         __Leaf
--      /             \               /            \       /  
--   _Leaf          _Leaf_         _Leaf_        _Leaf   None
--  /              /      \       /      \      /     
-- None          None    None   None   None   None     
--
p_tree_organization :: [Rect] -> Bool
p_tree_organization rects =
  let tree = foldl insert empty rects in
  checkTree tree

-- siblings MUST have been instantiated with the same data constructor 
checkTree :: HilbertRTree -> Bool
checkTree tree =
  case tree of
    None -> True
    Interior [] -> False
    Interior (NodeEntry { children = None}:_) -> False
    -- Interior case 1 : must be all leaves
    Interior ((n1@NodeEntry { children = Leaf _}):entries) ->
      foldl (\soFar entry -> soFar && case entry of
                NodeEntry { children = Interior _ } -> False
                NodeEntry { children = l@(Leaf _) } -> checkTree l
                NodeEntry { children = None } -> False) True (n1:entries)
    -- Interior case 2 :  must be all interior
    Interior ((n1@NodeEntry { children = Interior _ }):entries) ->
      foldl (\soFar entry -> soFar && case entry of
                NodeEntry { children = i@(Interior _) } -> checkTree i
                NodeEntry { children = Leaf _ } -> False
                NodeEntry { children = None } -> False) True (n1:entries)
    Leaf [] -> False
    Leaf (NodeEntry { children = Leaf _ }:_) -> False
    Leaf (NodeEntry { children = Interior _ }:_) -> False
    -- Leaves can only contain None's
    Leaf (NodeEntry { children = None }:entries) ->
      foldl (\soFar entry -> soFar && case entry of
                NodeEntry { children = Interior _ } -> False
                NodeEntry { children = Leaf _ } -> False
                NodeEntry { children = None } -> True) True entries

--helpers
insertMultiResult :: (Ord a, Num a, Ord b, Num b) => Rect -> a -> b
insertMultiResult rect x =
  fromIntegral $ length $ search (insertEmpty rect x) rect

insertEmpty :: (Ord a, Num a) => Rect -> a -> HilbertRTree
insertEmpty rect x = fillRec rect x empty

fillRec :: (Ord a, Num a) => Rect -> a -> HilbertRTree -> HilbertRTree
fillRec rect x tree | x == 0 = tree
                    | x > 0  = fillRec rect (x-1) (insert tree rect)
                    | otherwise = undefined
                                  
main :: IO ()
main = do
  quickCheck p_overlap_self
  quickCheck p_bound_self
  quickCheck p_bound_assoc
  quickCheck p_insert_in_empty
  quickCheck p_search_limit
  quickCheck p_insert_diff_result
  quickCheck p_insert_many_diff_result
  quickCheck p_tree_organization
