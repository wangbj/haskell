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

type Node = Int
type Weight = Int64

type Graph a b = Array a [(a, b)]

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

mkLefts lo hi xs = runSTUArray $ do
  left <- newArray (lo, hi) False :: ST s (STUArray s Int Bool)
  mapM_ (\k -> writeArray left k True) xs
  return $! left

{-# INLINE isLeftNode #-}
isLeftNode u x = (u ! x)

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

splitNodes = foldr go ([], []) . zip [1..]
  where go (!k, !b) (!left, !right)
          | b == 1 = (k : left, right)
          | otherwise = (left, k : right)

readEdges k = map readP3 . C.lines

readint = fst . fromJust . C.readInt

data MyGraph = MyGraph {
    _source :: {-# UNPACK #-} !Int
  , _sink :: {-# UNPACK #-} !Int
  , _left_size :: {-# UNPACK #-} !Int  -- Set.size is O(n)
  , _right_size :: {-# UNPACK #-} !Int  -- Set.size is O(n)    
  , _left ::  !(UArray Int Bool)
  , _right :: !IntSet
  , _graph :: !(Graph Node Weight)
  } deriving Show

mkGraph begin end undirected directed = runST $ do
  iou <- newArray (begin, end) [] :: ST s (STArray s Int [(Int, Int64)])
  mapM_ (\(from, to, weight) -> 
          readArray iou from >>= \fwd ->
          writeArray iou from ( (to, weight):fwd) >>
          readArray iou to >>= \rsd ->
          writeArray iou to ( (from, weight):rsd) ) undirected
  mapM_ (\(from, to, weight) ->
          readArray iou from >>= \dir ->
          writeArray iou from ( (to, weight):dir) ) directed
  unsafeFreeze iou

buildGraph :: C.ByteString -> MyGraph
buildGraph s = MyGraph src sink (length lefts) (length rights) leftu (IntSet.empty) gr
  where (!l1, !s1) = C.break (== '\n') s
        (!l2, !s2) = C.break (== '\n') (C.tail s1)
        (!numNodes, !numEdges) = readP2 l1
        (!lefts, !rights) = splitNodes (map readint (C.words l2))
        !leftu = mkLefts src sink (src:lefts)
        edges = filter (\(from, to, w) -> not (isLeftNode leftu from && isLeftNode leftu to)) . readEdges numEdges . C.tail $ s2
        extEdges = extendGraph src sink lefts rights
        !sink = 1 + numNodes
        src = 0
        gr = mkGraph src sink edges extEdges :: Graph Int Int64

extendGraph src sink lefts rights = fromSrcs
  where fromSrcs = map (\x -> (0, x, 0)) lefts

cnt (MyGraph source sink leftsize rightsize lefts rights graph) = if n < rightsize then Nothing else Just $! w
  where !wps = dijkstra source graph
        go (!r, !n) (WP weight node prevWeight prevNode)
          | isLeftNode lefts node = (r, n)
          | otherwise = (r + (weight - prevWeight), succ n)
        (!w, !n) = foldl go (0, 0) wps

pr Nothing = putStrLn "impossible"
pr (Just x) = print x

main = C.getContents >>= pr . cnt . buildGraph

--
ex1 = C.pack . unlines $ [
    "5 7"
  , "0 1 0 1 0"
  , "1 2 11"
  , "1 3 1"
  , "1 5 17"
  , "2 3 1"
  , "3 5 18"
  , "4 5 3"
  , "2 4 5"  ]  

ex2 = C.pack . unlines $ [
    "7 6"
  , "0 1 0 1 0 1 0"
  , "2 4 5"
  , "4 6 2"
  , "1 2 1"
  , "1 3 3"
  , "3 5 5"
  , "5 7 4"   ]

ex3 = C.pack . unlines $ [
    "7 7"
  , "0 1 0 1 0 1 0"
  , "2 4 5"
  , "4 6 2"
  , "1 2 1"
  , "1 3 3"
  , "3 5 5"
  , "7 6 3"
  , "5 7 4"   ]


