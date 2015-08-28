{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Set(Set)
import Data.IntSet(IntSet)
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unsafe
import Control.Monad.ST
import Data.Maybe
import Data.Int

type Node = Int
type Weight = Int

type Graph = Array Node [ (Node, Weight) ]

addEdge from heap (to, toWeight) = Set.insert (toWeight, to, from) heap
{-# INLINE addEdge #-}

-- insert non visited edges into the heap.
adjust :: IntSet -> Set (Weight, Node, Node) -> Node -> Graph -> Set (Weight, Node, Node)
adjust notVisited heap from graph = foldl (addEdge from) heap . filter (\(n, w) -> n `IntSet.member` notVisited) . (graph !) $ from

expand solution notVisited heap graph
  | IntSet.null notVisited = solution
  | otherwise = case Set.minView heap of
    Nothing -> error "heap empty, not strongly connected graph?"
    Just !(!(!cost, !to, !from), !heap') -> case IntSet.member to notVisited of
      True -> let !heap'' = adjust notVisited' heap' to graph
                  !notVisited' = IntSet.delete to notVisited
                  !solutions' = (cost, to, from) : solution
              in expand solutions' notVisited' heap'' graph
      False -> expand solution notVisited heap' graph

prim p gr = expand [] notVisited heap gr
  where (lo, hi) = bounds gr
        notVisited = IntSet.delete p . IntSet.fromList $ [lo..hi]
        !heap = adjust notVisited Set.empty p gr


cnt :: [(Weight, Node, Node)] -> Int64
cnt = foldl acc 0
  where acc r (!w, _, _) = r + (fromIntegral w)

readint = fst . fromJust . C.readInt
readP3 s = fromJust $ C.readInt s >>= \(x, s1) ->
  (C.readInt . C.tail) s1 >>= \(y, s2) ->
  (C.readInt . C.tail) s2 >>= \(z, _) ->
  return $! (x, y, z)

data Input = Input {
    __price    :: {-# UNPACK #-} !Int
  , __numNodes :: {-# UNPACK #-} !Int
  , __numEdges :: {-# UNPACK #-} !Int
  , __graph    :: ! Graph
  } deriving Show

buildGraph numNodes numEdges edges = runST $ do
  st <- newArray (1, numNodes) [] :: ST s (STArray s Node [ (Node, Weight) ])
  mapM_ (\(from, to, weight) ->
          readArray st from >>= \fwd ->
          writeArray st from ( (to, weight): fwd ) >>
          readArray st to >>= \rsd ->
          writeArray st to ( (from, weight) : rsd) ) edges
  unsafeFreeze st

mkGraph numNodes numEdges = go [] 1
  where go r k s
          | k > numEdges = (buildGraph numNodes numEdges r, s)
          | otherwise = let (!l1, !s1) = C.break (== '\n') s
                            edge@(!from, !to, !weight) = readP3 l1
                        in go (edge : r) (succ k) (C.tail s1)

price :: Int -> Int64 -> Int64
price u x = (fromIntegral u) * (fromIntegral x)

process input@(Input unitPrice numNodes numEdges gr) = print . price unitPrice . cnt . prim 1 $ gr

processinputs 0 _ = return ()
processinputs !k !s = process (Input unitPrice numNodes numEdges gr) >> processinputs (pred k) rest
  where (!l1, !s1) = C.break (== '\n') s
        (!l2, !s2) = C.break (== '\n') (C.tail s1)
        (!l3, !s3) = C.break (== '\n') (C.tail s2)
        !unitPrice = readint l1
        !numNodes = readint l2
        !numEdges = readint l3
        (!gr, !rest) = mkGraph numNodes numEdges (C.tail s3)

processall inputs = processinputs (readint l1) (C.tail s)
  where (l1, s) = C.break (== '\n') inputs

main = C.getContents >>= processall

---

ex1 = C.pack . unlines $ [
    "1"
  , "2"
  , "5"
  , "7"
  , "1 2 1"
  , "2 3 2"
  , "2 4 6"
  , "5 2 1"
  , "5 1 3"
  , "4 5 2"
  , "3 4 3"   ]  
