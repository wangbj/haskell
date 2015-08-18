{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.ByteString.Builder
import Data.IntMap.Strict(IntMap)
import Data.IntSet(IntSet)
import Data.Maybe
import Data.Monoid
import System.IO(stdout)

data St = Neither | Homo | Heter | Both

instance Show St where
  show Neither = "neither"
  show Homo = "homo"
  show Heter = "hetero"
  show Both = "both"

data H = H {
    _size :: {-# UNPACK #-} !Int
  , _mapSize :: {-# UNPACK #-} !Int  
  , _map :: ! (IntMap Int)
  , _set :: !IntSet
} deriving Show

empty = H 0 0 IntMap.empty IntSet.empty
insert k (H z1 z2 m s) = case IntMap.lookup k m of
  Nothing -> H (succ z1) (succ z2) (IntMap.insert k 1 m) s
  Just v -> if v == 1 then H (succ z1) z2 (IntMap.update (Just . succ) k m) (IntSet.insert k s)
            else H (succ z1) z2 (IntMap.update (Just . succ) k m) s

delete k h@(H z1 z2 m s) = case IntMap.lookup k m of
  Nothing -> h
  Just v -> case v of
    1 -> H (pred z1) (pred z2) (IntMap.delete k m) s
    2 -> H (pred z1) z2 (IntMap.update (Just . pred) k m) (IntSet.delete k s)
    u -> H (pred z1) z2 (IntMap.update (Just . pred) k m) s

status !(H z1 z2 m s)
  | IntMap.null m = Neither
  | z2 == 1 = if IntSet.null s then Neither else Homo
  | IntSet.null s = Heter
  | otherwise = Both

data OP = Insert !Int | Delete !Int
        deriving Show

readop s = case C.head w1 of
  'i' -> Insert k
  'd' -> Delete k
  where (!w1, s1) = C.break (== ' ') s
        !k = readint (C.tail s1)
        readint = fst . fromJust . C.readInt
        
eval = fst . foldl go (mempty, empty)
  where go (!z, !m) (Insert k) = let !m' = insert k m; !st = status m'
                                 in (z <> string8 (show st) <> char8 '\n', m')
        go (!z, !m) (Delete k) = let !m' = delete k m; !st = status m'
                                 in (z <> string8 (show st) <> char8 '\n', m')

process = hPutBuilder stdout . eval . map readop . tail . C.lines

main = C.getContents >>= process
