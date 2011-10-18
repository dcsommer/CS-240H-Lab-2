module Internal where

import Data.List as L
import Rect
import Control.Exception
import System.Exit

-- Types
--------

-- A minimum bounding rectangle (MBR) is a rectangle
type MBR = Rect

-- The return data type from handleOverflow
-- Can either be a list of siblings or a list of siblings
-- plus the newly created node, if overflow happened
data Return a = Return0 | Return1 [a] | Return2 [a] a

------- HilbertRTree

data NodeEntry = NodeEntry { mbr :: MBR
                           , children :: HilbertRTree
                           , lhv :: LHV
                           } deriving Show
                 
instance Eq NodeEntry where
         (==) a b = (lhv a) == (lhv b)

instance Ord NodeEntry where
         (<=) a b = (lhv a) <= (lhv b)

-- Leaves hold at most C_l entries, which have undefined children
-- Nodes hold at most C_n entries
data HilbertRTree = None | Leaf [NodeEntry] | Interior [NodeEntry]
                  deriving Show

empty = None
c_l = 2 :: Int          -- number of entries in a leaf node
c_n = 2 :: Int          -- number of entries in a non-leaf node

----------------------
------ Private helpers
----------------------

-- Given an entry to insert and a pair of (to insert into, siblings),
-- insertI returns the whole sibling group (Return1 [a]), or the whole
-- sibling group plus a new node (Return2 [a] a), if splitting was needed
insertI :: NodeEntry -> (NodeEntry, [NodeEntry]) -> Return NodeEntry
-- Case 1: Inserting into node whose children are leaves
insertI toInsert (NodeEntry { mbr = eMbr
                            , children = l@(Leaf eRects)
                            , lhv = eLhv }, siblings) =
  if (length eRects < c_l)
  --Fits in leaf node
  then Return1 ((NodeEntry { mbr = boundRects (mbr toInsert) eMbr
                           , children = Leaf (toInsert:eRects)
                           , lhv = max (lhv toInsert) eLhv }):siblings)
  --Doesn't fit, need to overflow to siblings or create new node
  else handleOverflow toInsert l siblings
       
-- Case 2: Inserting into node whose children are also non-leafs
insertI toInsert (NodeEntry { children = i@(Interior eNodes) },
                  siblings) =
  case insertI toInsert $ algC3 eNodes (lhv toInsert) of
    Return1 nodes -> Return1 ((liftInteriorEntries nodes):siblings)
    Return2 nodes new ->
      if (length siblings + 1) < c_n
      --This interior node has space for the s+1'th sibling
      then Return1 (new:(liftInteriorEntries nodes):siblings)
      --This node doesn't have space for the s+1'th sibling
      else handleOverflow new i siblings

--Turns a list of entries (contained in some internal node) into an
--entry. The return value can be inserted into the tree hierarchy
--at some point 
liftInteriorEntries :: [NodeEntry] -> NodeEntry
liftInteriorEntries es = assert (length es > 0)
  NodeEntry { mbr = maxMbr es
            , children = Interior es
            , lhv = maxLhv es }

--Same as above, but put into a leaf node, not an interior node
liftLeafEntries :: [NodeEntry] -> NodeEntry
liftLeafEntries es = assert (length es > 0)
  NodeEntry { mbr = maxMbr es
            , children = Leaf es
            , lhv = maxLhv es }

maxMbr (e:es) =
  foldl (\m e@NodeEntry{mbr = cur} -> boundRects m cur) (mbr e) es
maxLhv (e:es) =
  foldl (\m e@NodeEntry{lhv = cur} -> max m cur) (lhv e) es 

-- Returns a pair of (entry to insert into, the other entries) given a
-- list of entries in a node and the hilbert value of the item to insert
algC3 :: [NodeEntry] -> LHV -> (NodeEntry, [NodeEntry])
algC3 (e:es) h =
  foldl (\(a@NodeEntry { lhv = aLhv }, others)
          b@NodeEntry { lhv = bLhv }
         -> if (aLhv <= h)
            --We need to take the larger one
            then if (aLhv < bLhv)
                 then (b, L.insert a others) 
                 else (a, L.insert b others)
            --We take the smallest greater than h
            else if (h < bLhv && bLhv < aLhv)
                 then (b, L.insert a others)
else (a, L.insert b others)) (e, []) es

-- Insert in the overflow case
-- Given the entry to insert (r), 'n' the node we insert into
-- (containg either leaf or interior entries) [note that it should always
-- hold that (length entries == c_x), since we tried to insert r into
-- entries and failed, forcing a call to handleOverflow], and the s-1
-- cooperative siblings to 'n', returns a list of s siblings or a list of
-- s siblings and 1 new node (N N or P P)
handleOverflow :: NodeEntry -> HilbertRTree -> [NodeEntry] ->
                  Return NodeEntry
handleOverflow rect entry coopSib =
  case entry of
    Leaf entries -> let eps = epsilon entries in
      if length eps <= c_l * c_n
      then h3 eps $ nodesFor eps c_l
      else h4 eps $ nodesFor eps c_l
    Interior entries -> let eps = epsilon entries in
      if length eps <= c_n * c_n
      then h3 eps $ nodesFor eps c_n
      else h4 eps $ nodesFor eps c_n
    where
      nodesFor x cap = ceiling $ 
                       fromIntegral (length x) / fromIntegral cap
      -- Flat list of all peers of r
      epsilon entries = 
        sort $ foldl (\a b -> case children b of
                         Interior x -> (x++a)
                         Leaf x -> (x++a)) (rect:entries) coopSib
      -- Taken from paper, lines H3 and H4
      h3 toDistribute s = Return1 $ distributeOver entry s toDistribute
      h4 toDistribute s =
        let (x:xs) = distributeOver entry s toDistribute
        in Return2 xs x

-- Given a witness of the node type and the number of nodes to distribute
-- 'entries' over, we evenly split up 'entries' into 'numNodes' entries
distributeOver :: HilbertRTree -> Int -> [NodeEntry] -> [NodeEntry]
distributeOver witness numNodes entries = distRec numNodes entries []
  where distRec numNodes entries acc
          | numNodes == 0 = acc
          | otherwise     = assert (numNodes > 0)
                            distRec (numNodes-1) b (new:acc)
            where (a,b) = splitAt (div (length entries) numNodes) entries
                  new = case witness of
                    Leaf _     -> liftLeafEntries a
                    Interior _ -> liftInteriorEntries a