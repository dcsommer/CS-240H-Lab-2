import HilbertRTree
import Data.Word(Word16)
import System(getArgs)
import Control.Monad(liftM, mapM_)
import System.IO
import Text.Printf
import Data.Time.Clock

benchmark :: (a -> b) -> a -> IO (NominalDiffTime, b)
benchmark f a = do
  start <- getCurrentTime
  result <- return $ (f a) `seq` (f a)
  end <- getCurrentTime
  return (diffUTCTime end start, result)

min4 a b c d = min a $ min b $ min c d
max4 a b c d = max a $ max b $ max c d

toRect :: String -> Rect Word16
toRect str = makeRect $ map (\a -> read a :: Word16) $ splitOn ',' str
    where makeRect [x1,y1,x2,y2,x3,y3,x4,y4] =
              Rect { xlow  = min4 x1 x2 x3 x4
                   , xhigh = max4 x1 x2 x3 x4
                   , ylow  = min4 y1 y2 y3 y4
                   , yhigh = max4 y1 y2 y3 y4 }
          makeRect _                         =
              error ("Attempted to construct Rect, but \'" ++ str ++
                     "\' does not have exactly 8 integers in it")

maybeAdd a b = if (length a) > 0
               then b++[a] else b

splitOnRec :: Char -> String -> String -> [String] -> [String]
splitOnRec ch "" cur result = maybeAdd cur result
splitOnRec ch (x:rest) cur result
    | x == ch   = splitOnRec ch rest "" $ maybeAdd cur result
    | otherwise = splitOnRec ch rest (cur++[x]) result

splitOn :: Char -> String -> [String]
splitOn ch str = splitOnRec ch str "" []

-- A function that takes a list of Strings that can be made into
-- rectangles and builds a HilbertRTree by adding those rectangles
-- to the empty tree, one by one
buildTree :: [String] -> HilbertRTree Word16
buildTree = foldl (\a b -> insert a $ toRect b) $ Leaf 59432

printFound :: Show a => (NominalDiffTime, [Rect a]) -> IO ()
printFound (time, rects) = do
  printf "found %d matches in %s:\n" (length rects) (show time)
  mapM_ (\x -> putStr "    " >> print x) rects

handleQueries :: HilbertRTree Word16 -> IO ()
handleQueries hrt = do
  finished <- hIsEOF stdin
  if finished then return () else do
      inputLine <- getLine 
      found <- benchmark (search hrt) (toRect inputLine)
      printFound found  
      handleQueries hrt

main = do
  filename <- liftM head getArgs
  list <- liftM lines $ readFile filename

  --Benchmark the performance of constructing a HilbertRTree
  --We construct it by folding the list of rectangles from
  --the file with the insert function, starting with an empty HilbertRTree
  (readTime, hrt) <- benchmark buildTree list
  printf "%s: %d rectangles read in %s\n"
         filename (length list) (show readTime)
  handleQueries hrt
  return ()