{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet(IntSet)
import Data.IntMap.Strict(IntMap)
import Data.Set(Set)
import Control.Monad
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Data.Array.IO
import Data.Array.Unsafe

import Data.Maybe
import Data.Int
import Data.Ord
import Data.List
import Debug.Trace

type Node = Int
type Weight = Int
type LNode = (Node, Weight)

type Graph = Array Int [(Int, Int)]

data WP = WP {
    currWeight :: {-# UNPACK #-} !Weight
  , currNnode :: {-# UNPACK #-} !Node    
  , prevWeight :: {-# UNPACK #-} !Weight
  , prevNode :: {-# UNPACK #-} !Node    
  }

instance Show WP where
  show (WP w1 n1 w2 n2) = show n1 ++ "(" ++ show w1 ++ ")" ++ " <-- " ++ show n2 ++ "(" ++ show w2 ++ ")"
  
instance Eq WP where
  (WP cw1 cn1 pw1 pn1) == (WP cw2 cn2 pw2 pn2) = cw1 == cw2 && cn1 == cn2

instance Ord WP where
  compare (WP cw1 cn1 pw1 pn1) (WP cw2 cn2 pw2 pn2) = compare (cw1, cn1) (cw2, cn2)

data Work = Work {
    _done :: [WP]
  , _visited :: !IntSet
  , _minHeap :: !(Set WP)
  , _adjustedNodes :: !(IntMap WP)
  } deriving Show

{-# INLINE adjustHeap #-}
{-# INLINE insertHeap #-}
adjustHeap old new heap = Set.insert new . Set.delete old $ heap
insertHeap new = Set.insert new

{-# INLINE mkWP #-}
mkWP w n pw pn = WP w n pw pn

adjust heap adjusted from@(WP w1 n1 pw1 pn1) (!to, !weight) = case IntMap.lookup to adjusted of
  Nothing -> ((insertHeap (mkWP newWeight to w1 n1) heap), (IntMap.insert to (mkWP newWeight to w1 n1) adjusted))
  Just wpold@(WP oldWeight oldn oldw1 oldn1) -> case compare newWeight oldWeight of
    GT -> (heap, adjusted)
    EQ -> if weight >= (w1 - pw1) then (heap, adjusted) else
            ((adjustHeap wpold (mkWP newWeight to w1 n1) heap), (IntMap.insert to (mkWP newWeight to w1 n1) adjusted))
    LT -> ((adjustHeap wpold (mkWP newWeight to w1 n1) heap), (IntMap.insert to (mkWP newWeight to w1 n1) adjusted))
  where !newWeight = w1 + weight

{-# INLINE expand #-}
expand (!heap, !adjusted) (!latest, !to) = r --trace ("expand" ++ show (latest, to, r)) $ r
  where !r = adjust heap adjusted latest to

{-# INLINE mkSource #-}
mkSource srcs = Work done (IntSet.fromList srcs) Set.empty IntMap.empty
  where done = map (\s -> WP 0 s 0 s) srcs

sp work@(Work ( (latest@(WP startW startN prevW prevN)) : done) visited heap adjusted) graph = 
  let !adjs = filter (\(to, toW) -> to `IntSet.notMember` visited ) (graph ! startN)
      !(!heap', !adjusted') = foldl expand (heap, adjusted) (zip (repeat latest) adjs)
  in case Set.minView heap' of    
    Nothing -> (latest:done)
    Just (!wpnew@(WP minW minN prevMinW prevMinN), !heap'') -> 
      let !visited' = IntSet.insert minN visited
          !done' = wpnew : latest : done
      in sp (Work done' visited' heap'' adjusted') graph

dijkstra source graph = sp (mkSource [source]) graph

{-# INLINE readP2 #-}
{-# INLINE readP3 #-}
readP2 s = fromJust $ C.readInt s >>= \(x, s1) ->
  (C.readInt . C.tail) s1 >>= \(y, _) ->
  return $! (x, y)

readP3 s = fromJust $ C.readInt s >>= \(x, s1) ->
  (C.readInt . C.tail) s1 >>= \(y, s2) ->
  (C.readInt . C.tail) s2 >>= \(z, _) ->  
  return $! (x, y, (fromIntegral z))

mkGraph :: Node -> Node -> [(Int, Int, Int)] -> Graph
mkGraph begin end undirected = runST $ do
  iou <- newArray (begin, end) [] :: ST s (STArray s Int [(Int, Int)])
  mapM_ (\(from, to, weight) ->
          readArray iou from >>= \dir ->
          writeArray iou from ( (to, weight):dir) >>
          readArray iou to >>= \rsd ->
          writeArray iou to ( (from, weight):rsd) )  undirected
  unsafeFreeze iou

readint = fst . fromJust . C.readInt

processinputs (ne:rest) = mapM_ (runQuery mindist) queries
  where numEdges = readint ne
        (!ees, src:nq:qs) = splitAt numEdges rest
        !edges = map readP3 ees
        !queries = map readint qs
        !gr = mkGraph 0 500 edges
        !mindist = IntMap.fromList . map (\(WP w n w1 n1) -> (n, w)) . dijkstra (readint src) $ gr
        runQuery m q = case IntMap.lookup q m of
          Nothing -> putStrLn "NO PATH"
          Just d -> print d
        
processall txts = processinputs (C.lines txts)

main = C.getContents >>= processall


---
ex1 = C.pack . unlines $ [
    "7"
  , "0 1 4"
  , "0 3 8"
  , "1 4 1"
  , "1 2 2"
  , "4 2 3"
  , "2 5 3"
  , "3 4 2"
  , "0"
  , "4"
  , "1"
  , "4"
  , "5"
  , "7"   ]  
