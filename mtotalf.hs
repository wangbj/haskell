{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Data.IntMap.Strict(IntMap)
import Data.Map.Strict(Map)
import Data.IntSet(IntSet)
import Data.Sequence(Seq)
import Data.Tree
import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unsafe
import Control.Monad.ST
import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import Data.Char

type Node = Int
type Weight = Int

type Graph = Array Int (IntMap Weight)

toInt c
  | c >= 'A' && c <= 'Z' = 1 + (ord c - ord 'A')
  | c >= 'a' && c <= 'z' = 27 + (ord c - ord 'a')

readP3 s = fromJust $ C.uncons s >>= \(c1, s1) ->
  (C.uncons . C.tail) s1 >>= \(c2, s2) ->
  (C.readInt . C.tail ) s2 >>= \(x, _) ->
  return $! (toInt c1, toInt c2, x)

srcNode = 1 :: Int
dstNode = 26 :: Int

readGraph :: C.ByteString -> Graph
readGraph inputs = runST $ do
  stu <- newArray (1, 52) IntMap.empty :: ST s (STArray s Node (IntMap Weight))
  let edges = map readP3 . tail . C.lines $ inputs
  mapM_ (\(from, to, weight) -> readArray stu from >>= \fwd ->
          writeArray stu from (IntMap.insertWith (+) to weight fwd) ) edges
  unsafeFreeze stu

toResidual gr = go lo Map.empty
  where (lo, hi) = bounds gr
        go k m
          | k > hi = m
          | otherwise =
            let !neb = gr ! k
                !m' = IntMap.foldlWithKey (\t to w -> Map.insert (k, to) w t) m neb
            in go (succ k) m'

mkP3 from [] = []
mkP3 from ( (to, w): tos ) = ( (from, to), w ) : mkP3 from tos

expand g sink k = Identity $ if k == sink then (k, []) else (k, adjs)
  where adjs = IntMap.keys (g ! k)

debugPath g src sink = putStr . drawTree . runIdentity . T.mapM (Identity . show) . runIdentity . unfoldTreeM_BF (expand g sink) $ src

walk g sink path (r, res) = if last path /= sink then (r, res) else if mf == 0 then (r, res) else (r+mf, res')
  where !edges = zip (init path) (tail path)
        !flows = map (\e -> maybe 0 id (Map.lookup e res)) $ edges
        !mf = minimum flows
        res' = foldl (go mf) res edges
        go f r e = case Map.lookup e r of
          Nothing -> r
          Just oldf -> Map.update (\f0 -> if f >= f0 then Nothing else Just $! (f0 - f)) e r

walkForest :: [Tree a] -> [[a]]
walkForest [] = [[]]
walkForest ts = concatMap walkTree ts
walkTree :: Tree a -> [[a]]
walkTree (Node x ts) = map (x:) (walkForest ts)

edmondKarp_ g src sink residual = fst . foldr (walk g sink) (0, residual) $ path
  where path = walkTree . runIdentity . unfoldTreeM_BF (expand g sink) $ src

edmondKarp src sink graph = edmondKarp_ graph src sink residual
  where !nexts = Seq.singleton src
        !residual = toResidual graph

main = C.getContents >>= print . edmondKarp 1 26 . readGraph

---
ex1 = C.pack . unlines $ [
    "5"
  , "A B 3"
  , "B C 3"
  , "C D 5"
  , "D Z 4"
  , "B Z 6"    ]

ex2 = C.pack . unlines $ [
    "5"
  , "A B 3"
  , "B A 3"
  , "C D 5"
  , "D Z 4"
  , "B Z 6"    ]

ex3 = C.pack . unlines $ [
    "8"
  , "A B 10"
  , "A C 8"
  , "B C 2"
  , "B D 5"
  , "C E 4"
  , "C D 6"
  , "D Z 10"
  , "E Z 20"   ]
    
