module Trie (
    lookup
  , insert
  , insert'
  , insertWith
  , toList
  ) where

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe
import Control.Monad
import Data.Char
import Prelude hiding (lookup)

data Trie a = Trie {
    value :: Maybe a
  , children :: Map Char (Trie a)
  } deriving (Show, Eq)

empty = Trie Nothing Map.empty
singleton c = insert [c] empty

lookup [] (Trie v _) = True
lookup (c:cs) t@(Trie val child) = case Map.lookup c child of
  Nothing -> False
  Just child' -> lookup cs child'

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
