module HilbertRTree(HilbertRTree, HilbertRTree.insert, search, empty) where

import Rect
import Data.Word(Word16)
import Data.List as L
import Control.Exception(assert)

-- Constants
------------
word16Max = (-1 :: Word16) --This becomes (2^16 - 1 == 65535)
maxOverlap = 4     --This is the maximum number of results returned by
                   --  `search`
             
c_l = 4            -- number of entries in a leaf node
c_n = 3            -- number of entries in a non-leaf node

-- Types
--------

-- The Largest Hilbert Value (LHV) is really a Word16
type LHV = Word16

-- A minimum bounding rectangle (MBR) is a rectangle
type MBR = Rect

-- The return data type from handleOverflow
-- Can either be a list of siblings or a list of siblings
-- plus the newly created node, if overflow happened
data Return a = Return0 | Return1 [a] | Return2 [a] a

------- HilbertRTree

data NodeEntry = NodeEntry { mbr :: MBR
                           , child :: HilbertRTree
                           , lhv :: LHV
                           }
                 
instance Eq NodeEntry where
         (==) a b = (lhv a) == (lhv b)

instance Ord NodeEntry where
         (<=) a b = (lhv a) <= (lhv b)

-- Leaves hold at most C_l entries, which have undefined children
-- Nodes hold at most C_n entries
data HilbertRTree = Leaf [NodeEntry] | Interior [NodeEntry]

empty = Interior []

insert :: HilbertRTree -> Rect -> HilbertRTree
-- Inserting into a leaf is not defined. No leaf should be passed into
-- `insert`. This should be fine because clients never get a handle to
-- a Leaf        
insert (Leaf val) rect = undefined
-- First, base case for an empty HRT
insert a@(Interior entries) rect | length (entries) == 0 =
  Interior [NodeEntry rect (Leaf [toInsert]) h]
-- Inserting into a non-empty interior node
                              | otherwise =
  case insertI $ algC3 entries h of
    Return1 a -> Interior a
    Return2 a b -> Interior $ i4 a b
  where h = hilbertValue rect
        toInsert = NodeEntry { mbr = rect, child = undefined, lhv = h}
        -- Takes the pair of (entry to modify, cooperating siblings)
        -- Returns the whole sibling group, which may be greater than
        -- capacity, necessating further splitting
        insertI :: (NodeEntry, [NodeEntry]) -> Return NodeEntry
        -- Case 1: Inserting into node whose children are leaves
        insertI (n@NodeEntry { mbr = eMbr
                             , child = l@(Leaf eRects)
                             , lhv = eLhv }, siblings) =
          if (length eRects < c_l)
          --Fits in leaf node
          then Return1 ((NodeEntry { mbr = boundRects rect eMbr
                                   , child = Leaf (toInsert:eRects)
                                   , lhv = max h eLhv }):siblings)
          --Doesn't fit, need to overflow to siblings or create new node
          else handleOverflow toInsert l siblings
               
        -- Case 2: Inserting into node whose children are also non-leafs
        insertI (NodeEntry { child = i@(Interior eNodes) }, siblings) =
          case insertI $ algC3 eNodes h of
            Return1 nodes -> Return1 ((makeEntry nodes):siblings)
            Return2 nodes new ->
              if (length siblings + 1) < c_n
              --This interior node has space for the s+1'th sibling
              then Return1 (new:(makeEntry nodes):siblings)
              --This node doesn't have space for the s+1'th sibling
              else handleOverflow new i siblings

search :: HilbertRTree -> Rect -> [Rect]
search (Leaf children) rect = foldl searchF [] children where
  searchF soFar NodeEntry{ mbr = x } = 
    if (length soFar < maxOverlap) && (intersectRect x rect)
    then x:soFar
    else soFar
search (Interior children) rect = foldl searchF [] children where
  searchF soFar NodeEntry{ mbr = x, child = c } = 
    if (length soFar < maxOverlap) && (intersectRect x rect)
    then soFar ++ search c rect
    else soFar

------ Private helpers


--Given a list of entries constituting the old root of the tree
--and a new sibling to that root, returns a new list of entries
--for the root
i4 :: [NodeEntry] -> NodeEntry -> [NodeEntry]
i4 old new = new:[makeEntry old]
                            
--Turns a list of entries (contained in some internal node) into an
--entry. Once an entry, it can be readily inserted into the tree hierarchy
--at some point 
makeEntry :: [NodeEntry] -> NodeEntry
makeEntry (e:es) =
  NodeEntry { mbr = foldl (\m e@NodeEntry{mbr = cur} ->
                            boundRects m cur) (mbr e) es
            , child = Interior (e:es)
            , lhv = foldl (\m e@NodeEntry{lhv = cur} ->
                            max m cur) (lhv e) es }

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
      then h3 eps
      else h4 eps
    Interior entries -> let eps = epsilon entries in
      if length eps <= c_n * c_n
      then h3 eps
      else h4 eps    
    where
      -- s, the number of siblings (before any splits)
      s = 1 + length coopSib
      -- Flat list of all peers of r
      epsilon entries = 
        sort $ foldl (\a b -> case child b of
                         Interior x -> (x++a)
                         Leaf x -> (x++a)) (rect:entries) coopSib
      -- Taken from paper, lines H3 and H4
      h3 toDistribute = Return1 $ distributeOver s toDistribute
      h4 toDistribute =
        let (x:xs) = distributeOver (s+1) toDistribute in Return2 xs x
      -- Given a number of nodes to distribute 'entries' over,
      -- evenly split up 'entries' into 'numNodes' entries
      distributeOver numNodes entries = distRec numNodes entries []
        where
          distRec numNodes entries acc = distRec (numNodes-1) b (new:acc)
          (a,b) = splitAt (div (length entries) numNodes) entries
          new = makeEntry a
