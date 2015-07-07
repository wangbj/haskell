{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU
import Data.Maybe(fromJust)
import Data.Array.Unboxed
import Data.Array.IO
import Control.Monad

data Item = Item {
    _i_weight :: {-# UNPACK #-} !Int
  , _i_value :: {-# UNPACK #-} !Int
  } deriving Show

knapsack cap items = m ! (itemsize, cap)
  where go 0 _ = 0
        go _ 0 = 0
        go !i !j
          | w <= j = max (m ! (i-1, j)) (  (m!(i-1, j-w))+v)
          | otherwise = m ! (i-1, j)
          where !w = iu ! i
                !v = iv ! i
        m = listArray ((0,0), (itemsize, cap)) [ go i j | i <- [0..itemsize], j <- [0..cap]] :: Array (Int, Int) Int
        iu = listArray (1, length items) (map _i_weight items) :: UArray Int Int
        iv = listArray (1, length items) (map _i_value items) :: UArray Int Int
        !itemsize = length items

knapsack_ cap items = do
  let itemsize = length items
      iu = listArray (1, itemsize) (map _i_weight items) :: UArray Int Int
      iv = listArray (1, itemsize) (map _i_value items) :: UArray Int Int
      go _ 0 _ = return ()
      go _ _ 0 = return ()
      go m i j
        | w <= j = readArray m (pred i, j) >>= \v1 -> readArray m (pred i, j-w) >>= \v2 -> writeArray m (i,j) (max v1 (v2+v))
        | otherwise = readArray m (pred i, j) >>= \v -> writeArray m (i, j) v
        where !w = iu ! i
              !v = iv ! i
      go :: (IOUArray (Int, Int) Int) -> Int -> Int -> IO ()
  iou <- newArray ( (0, 0), (itemsize, cap)) 0 :: IO (IOUArray (Int, Int) Int)
  forM_ [1..itemsize] (\i ->
    forM_ [1..cap] (\j ->
                     go iou i j ) )
  readArray iou (itemsize, cap)
                     

readi2_ s = C.readInt s >>= \(i1, s1) ->
  C.readInt (BU.unsafeTail s1) >>= \(i2, _) ->
  return $! Item i1 i2

readi2 = fromJust . readi2_
{-# INLINE readi2 #-}

process inputs = knapsack_ cap items >>= print
  where ((Item cap _): items) = map readi2 (C.lines inputs)

main = C.getContents >>= process
