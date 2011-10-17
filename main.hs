import HilbertRTree
import Rect
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

sampleRect = toRect "1,1,2,1,1,2,2,2"

toRect :: String -> Rect
toRect str = makeRect $ map (\a -> read a :: Word16) $ splitOn ',' str
    where makeRect (x:y:rest) = makeRectR (Rect x x y y) rest
          makeRect _          =
            error ("Attempted to construct Rect, but \'" ++ str ++
                   "\' does not even have 2 integers in it")
          makeRectR cur (x:y:rest) =
            makeRectR (boundRects cur (Rect x x y y)) rest
          makeRectR cur _ = cur

splitOn :: Char -> String -> [String]
splitOn ch str = reverse $ splitOnRec ch str "" [] where
  splitOnRec :: Char -> String -> String -> [String] -> [String]
  splitOnRec ch "" cur result = maybeAdd cur result
  splitOnRec ch (x:rest) cur result
    | x == ch   = splitOnRec ch rest "" $ maybeAdd cur result
    | otherwise = splitOnRec ch rest (x:cur) result
  maybeAdd a b = if (length a) > 0
                 then (reverse a):b else b

-- A function that takes a list of Strings that can be made into
-- rectangles and builds a HilbertRTree by adding those rectangles
-- to the empty tree, one by one
buildTree :: [String] -> HilbertRTree
buildTree = foldl (\a b -> insert a $ toRect b) $ empty

printFound :: (NominalDiffTime, [Rect]) -> IO ()
printFound (time, rects) = do
  printf "found %d matches in %s:\n" (length rects) (show time)
  mapM_ (\x -> putStr "    " >> print x) rects

handleQueries :: HilbertRTree -> IO ()
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