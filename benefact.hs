{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.IntMap.Strict(IntMap)
import Data.IntSet(IntSet)
import Data.Sequence(Seq)
import Control.Monad.State
import Data.Monoid
import Data.Maybe
import Data.Int

data Max' a = Max' {
  getMax' :: Maybe a
  } deriving Show

instance (Ord a) => Monoid (Max' a) where
  mempty = Max' Nothing
  (Max' a) `mappend` (Max' b) = Max' (max a b)
  
newtype Max a = Max {
  getMax :: a
  } deriving (Eq, Ord, Show, Bounded, Functor)

newtype Min a = Min {
  getMin :: a
  } deriving (Eq, Ord, Show, Bounded, Functor)

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound
  Max a `mappend` Max b = Max (max a b)

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min minBound
  Min a `mappend` Min b = Min (min a b)

type Node = Int
type LNode a = (Node, a)
type Edge = (Int, Int)
type LEdge a = (Edge, a)

newtype Graph a = Graph {
  unGraph :: IntMap (Seq (LNode a))
  }

instance (Show a) => Show (Graph a) where
  show graph = show $ (unGraph graph)

empty = Graph IntMap.empty

addEdge s t w = Graph . IntMap.insertWith (\old new -> old <> new) t (Seq.singleton (s, w)) . IntMap.insertWith (\old new -> old <> new) s (Seq.singleton (t, w)) . unGraph

g1 :: Graph Int
g2 :: Graph Int
g1 = addEdge 6 5 5 . addEdge 6 4 6 . addEdge 2 6 2 . addEdge 2 3 4 . addEdge 1 2 3 $ empty
g2 = addEdge 1 2 7 . addEdge 2 3 1 . addEdge 2 6 6 . addEdge 6 4 1 . addEdge 6 5 2 . addEdge 3 7 1 $ empty

mkGraph :: [LEdge a] -> Graph a
mkGraph = foldl acc empty
  where acc g (!(!s, !t), !w) = addEdge s t w g

bfsg_ f g !o (Seq.viewl -> Seq.EmptyL) = return o
bfsg_ f g !o (Seq.viewl -> (!(!s, !p), !(!l, !z)) Seq.:< q) = get >>= \ss -> case s `IntSet.member` ss of
  True -> bfsg_ f g o q
  False -> do
    put (IntSet.insert s ss)
    let !notVisited' = fmap (\(x, y) -> ((x,s), (1 + l, fromIntegral y + fromIntegral z))) . Seq.filter (\(x, y) -> x `IntSet.notMember` ss) $ (g IntMap.! s)
    bfsg_ f g (f s p l z <> o) (q <> notVisited')

costs :: Int -> Int -> Int -> Int64 -> Max Int64
costs s p l w = Max w

bfsg f z g = evalState (bfsg_ f (unGraph g) mempty q ) IntSet.empty
  where q = Seq.singleton ( (z, z), (1, 0) )

readint = fst . fromJust . C.readInt

readLEdge s = fromJust $ C.readInt s >>= \(x, s1) ->
  (C.readInt . C.tail) s1 >>= \(y, s2) ->
  (C.readInt . C.tail )s2 >>= \(z, _) ->
  return $! ( (x, y), z )

process g = print . go $ s
  where s = snd . fromJust . getMax' . bfsg (\s p l w -> Max' (Just (w, s))) 1 $ g
        go s = getMax . bfsg costs s $ g

processinputs 0 _ = return ()
processinputs k (n:s) = process gr >> processinputs (pred k) s'
  where (s1, s') = splitAt ((readint n) - 1) s
        gr = mkGraph . map readLEdge $ s1

processall inputs = processinputs (readint nt) s
  where (nt:s) = C.lines inputs

main = C.getContents >>= processall
