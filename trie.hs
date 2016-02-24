{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.ByteString.Builder
import Control.Applicative hiding (empty)
import Control.Arrow
import Control.Monad.Writer
import Data.Monoid
import Data.Char
import System.IO (stdout)
import Data.Maybe

newtype Trie s a = Trie {
  unTrie :: (FiniteStream s) => IntMap (Maybe a, Trie s a)
  } 

empty = Trie IntMap.empty

class (Monoid s) => FiniteStream s where
  type FiniteStreamElementType s :: *
  uncons :: s -> Maybe (FiniteStreamElementType s, s)
  isEmpty :: s -> Bool
  fromListS :: [FiniteStreamElementType s] -> s
  toListS :: s -> [FiniteStreamElementType s]
  fromKey :: Int -> FiniteStreamElementType s
  toKey :: FiniteStreamElementType s -> Int

instance FiniteStream C.ByteString where
  type FiniteStreamElementType C.ByteString = Char
  uncons = C.uncons
  isEmpty = C.null
  fromListS = C.pack
  toListS = C.unpack
  fromKey = chr
  toKey = ord
  
instance (Bounded a, Integral a) => FiniteStream [a] where
  type FiniteStreamElementType [a] = a
  uncons [] = Nothing
  uncons (x:xs) = Just (x, xs)
  isEmpty [] = True
  isEmpty _ = False
  fromListS = id
  toListS = id
  fromKey = fromIntegral
  toKey = fromIntegral

member (uncons -> Nothing) _ = True
member (uncons -> Just (c, cs)) (Trie t) = case IntMap.lookup c t of
  Nothing -> False
  Just (_, tr) -> member cs tr

insertWith f (uncons -> Nothing) v t = t
insertWith f (uncons -> Just (c, cs)) v (Trie t)
  | isEmpty cs = case IntMap.lookup k t of
    Nothing -> Trie $ IntMap.insert k (Just v, empty) t
    Just _ -> Trie $ IntMap.update (Just . first (\x -> case x of
                                                      Nothing -> Just v
                                                      Just x' -> Just (f v x'))) k t
  | otherwise = case IntMap.lookup k t of
    Nothing -> Trie $ IntMap.insert k (Nothing, insertWith f cs v empty) t
    Just _ -> Trie $ IntMap.update (Just . fmap (insertWith f cs v) ) k t
  where k = c
          

insert s v = insertWith const s v
singleton k v = insertWith const k v empty

cons m t = mapM_ (uncurry (consM m)) (IntMap.assocs (unTrie t))
consM m k (Nothing, t) = cons (m<>pure (k)) t
consM m k (Just v, t) = let m' = m <> pure (k)
                        in tell (Seq.singleton (F.toList m', v)) >> cons m' t

assocs t = F.toList $ execWriter (cons Seq.empty t)

instance (FiniteStream s, Show a) => Show (Trie s a) where
  show t = show $ execWriter (cons Seq.empty t)
