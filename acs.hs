{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)

import Data.ByteString.Builder

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char
import Data.Monoid

import System.IO (stdout)

data MQ = R {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | C {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | Q {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | S {-# UNPACK #-} !Int

data Mapping = Mapping {
    rowMappingR :: !(IntMap Int)
  , rowMappingF :: !(IntMap Int)    
  , colMappingR :: !(IntMap Int)
  , colMappingF :: !(IntMap Int)    
  } 

initMapping = Mapping z z z z
  where z = IntMap.empty

(!!!) :: IntMap Int -> Int -> Int
(!!!) m k = case IntMap.lookup k m of
  Nothing -> k
  Just v -> v

update (Mapping rr rf cr cf) (R x y) = Mapping rr'' rf'' cr cf
  where x1 = rr !!! x
        x2 = rr !!! y
        rr' = IntMap.insert x x2 rr
        rr'' = IntMap.insert y x1 rr'
        rf' = IntMap.insert x2 x rf
        rf'' = IntMap.insert x1 y rf'
update (Mapping rr rf cr cf) (C x y) = Mapping rr rf cr'' cf''
  where y1 = cr !!! x
        y2 = cr !!! y
        cr' = IntMap.insert x y2 cr
        cr'' = IntMap.insert y y1 cr'
        cf' = IntMap.insert y2 x cf
        cf'' = IntMap.insert y1 y cf'
update m _ = m

nrows = 1234 :: Int
ncols = 5678 :: Int

locate :: Int -> (Int, Int)
locate x = (succ r, succ c)
  where (r, c) = (pred x) `quotRem` ncols

value :: Int -> Int -> Int
value i j = 1 + (i-1) * 5678 + (j-1)

readint = fst . fromJust . C.readInt
readpair c s = fromJust $
  (C.readInt . C.tail) s >>= \(x, s1) ->
  (C.readInt . C.tail) s1 >>= \(y, _) ->
  return $! c x y

readone c s = fromJust $
  (C.readInt . C.tail) s >>= \(x, _) ->
  return $! c x

readMQ (fromJust . C.uncons -> ('R', s1)) = readpair R s1
readMQ (fromJust . C.uncons -> ('C', s1)) = readpair C s1
readMQ (fromJust . C.uncons -> ('Q', s1)) = readpair Q s1
readMQ (fromJust . C.uncons -> ('W', s1)) = readone S s1

dec1 x = intDec x <> char8 '\n'
dec2 x y = intDec x <> char8 ' ' <> dec1 y

query :: Int -> Int -> State Mapping Builder
query i j = do
  ~(Mapping rr rf cr cf) <- get
  let x = rr !!! i
      y = cr !!! j
  return $! dec1 (value x y)

search :: Int -> State Mapping Builder
search n = do
  ~(Mapping rr rf cr cf) <- get
  let (i, j) = locate n
      x = rf !!! i
      y = cf !!! j
  return $! dec2 x y

acs :: [MQ] -> Builder
acs mq = evalState (foldM go mempty mq) initMapping
  where go z r@(R x y) = get >>= \mapping -> put (update mapping r) >> return z
        go z c@(C x y) = get >>= \mapping -> put (update mapping c) >> return z
        go z (Q x y) = fmap (z <>) (query x y)
        go z (S x) = fmap (z <>) (search x)

acs_ = hPutBuilder stdout . acs

getinputs = map readMQ . C.lines
main = C.getContents >>= acs_ . getinputs

ex1 = C.pack . unlines $ [
    "R 1 3"
  , "R 4 5"
  , "R 2 3"
  , "R 5 1"
  , "C 1 3"
  , "C 5 6"
  , "Q 2 3"
  , "Q 3 4"
  , "W 1"
  , "W 5679"
  , "Q 5 6"
  , "W 11357"
  , "W 7006652"  ]  
