{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import Data.Map.Strict(Map)
import Data.IntSet(IntSet)
import Data.Maybe
import Control.Monad
import Data.Char
import Prelude hiding (lookup)

data Trie a = Trie {
    value :: !(Maybe a)
  , children :: !(Map Char (Trie a))
  } deriving (Show, Eq)

empty = Trie Nothing Map.empty
singleton c = insert [c] empty

member [] (Trie v _) = True
member (c:cs) t@(Trie val child) = case Map.lookup c child of
  Nothing -> False
  Just child' -> member cs child'

size (Trie val child) = fromJust val

incr m = return $! case m of
  Nothing -> 1
  Just x -> succ x

decr m = return $! case m of
  Nothing -> Nothing
  Just x -> case x of
    1 -> Nothing
    x' -> Just $! pred x'

insert_ [] t = t
insert_ (c:cs) t@(Trie val child) = case Map.lookup c child of
  Nothing -> t { children = Map.insert c (insert_ cs empty) child, value = incr val }
  Just child'@(Trie v _) -> t {children = Map.insert c (insert_ cs child') child, value = incr val }

insertWith g s t@(Trie val child) = insert_ (g s) t

insert = insertWith id
insert' = insertWith (\s -> s ++ [nil])
  where nil = chr 0

toList t@(Trie val child)
  | Map.null child = [[]]
  | otherwise =  Map.foldrWithKey go [] child
  where go c t r = fmap (c:) (toList t) ++ r

--

data Diff a = Any | Match a
                    deriving Show

instance Eq a => Eq (Diff a) where
  Any == _ = True
  _ == Any = True
  Match x == Match y = x == y

--
match [] _ _ = True
match (c:cs) t@(Trie val child) d
  | Map.null child = False
  | otherwise = any (==True) $ map (uncurry (match cs)) (Map.foldlWithKey iter [] child)
    where iter r k v 
            | c == k && c == (chr 0) = ( (v, d): r)
            | d' == d = ((v, d'):r)
            | otherwise = r
            where !d' = Match (abs (ord c - ord k))

mkDict :: [C.ByteString] -> Trie Int
mkDict = foldl go empty
  where go dict s
          | dup == True = dict
          | dup == False = insert' s' dict
          where !s' = (C.unpack s) ++ [chr 0]
                !dup = match s' dict Any

eqPass = size . mkDict

readint = fst . fromJust . C.readInt

processinputs k n s
  | k > n = return ()
  | otherwise = let (l1, s1) = (head s, tail s)
                    (ss, rest) = splitAt (readint l1) s1
                in putStrLn ("Case " ++ show k ++ ": " ++ show (eqPass ss)) >> processinputs (succ k) n rest

processall s = processinputs 1 (readint l1) (C.words . C.tail $ s1)
  where (l1, s1) = C.break (== '\n') s

main = C.getContents >>= processall
