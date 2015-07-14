{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(inits)
import Data.Maybe
import Control.Monad
import Prelude hiding (lookup)

ex1 = C.pack "2\n3\n911\n97625999\n91125426\n5\n113\n12340\n123440\n12345\n98346\n"

data Trie a = Trie {
    value :: Maybe a
  , children :: Map Char (Trie a)
  } deriving (Show, Eq)

empty = Trie Nothing Map.empty

lookup [] (Trie v _) = True
lookup (c:cs) t@(Trie val child) = case Map.lookup c child of
  Nothing -> False
  Just child' -> lookup cs child'

insert [] t@(Trie val child) = t
insert (c:cs) t@(Trie val child) = case Map.lookup c child of
  Nothing -> t { children = Map.insert c (insert cs empty) child }
  Just child' -> t {children = Map.insert c (insert cs child') child }

-- no one is a prefix of others.
insert' [] t@(Trie val child) = case val of
  Nothing -> Just $! t { value = Nothing } -- end of word
  Just _ -> Nothing -- already in trie.
insert' (c:cs) t@(Trie val child) = case Map.lookup c child of
  Nothing -> insert' cs empty >>= \newChild -> return $! t { children = Map.insert c newChild child, value = incr val }
  Just !child'@(Trie v _) -> v >>= \v1 -> insert' cs child' >>= \newChild -> return $! t {children = Map.insert c newChild child, value = incr val }
  where incr m = return $! case m of
          Nothing -> 1
          Just !x -> succ x

phonelst = flip go empty
  where go [] _ = False
        go (s:ss) m = case insert' s m of
          Nothing -> True
          Just m' -> go ss m'

readint = fst . fromJust . C.readInt

process [] = return ()
process (h:rest) = go (readint h) rest empty
  where go k s m
          | k < 1 = putStrLn "YES" >> process s
          | otherwise = case insert' (C.unpack . head $ s) m of
            Nothing -> putStrLn "NO" >> process (drop k s)
            Just !m' -> go (pred k) (tail s) m'

main = C.getContents >>= process . tail . C.lines
            
