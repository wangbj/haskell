{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU
import Data.Maybe(fromJust)
import Data.Array.Unboxed
import Data.Array.IO
import Data.Array.Unsafe
import Control.Monad

maxp = 50000 :: Int
maxw = 10000 :: Int
maxv = 1000000000 :: Int

data Item = Item {
    weight :: {-# UNPACK #-} !Int
  , value :: {-# UNPACK #-} !Int
  }

knapsack_ ncap items = do
  iou <- newArray (0, ncap) maxv :: IO (IOUArray Int Int)
  writeArray iou 0 0
  forM_ items (\i ->
                forM_ [(weight i)..ncap] (\j ->
                                           readArray iou j >>= \v1 ->
                                           readArray iou (j-(weight i)) >>= \v2 ->
                                           writeArray iou j (min v1 (value i + v2))
                                         )
              )
  r <- readArray iou ncap
  if r >= maxv then putStrLn "This is impossible." else
                    putStrLn $ "The minimum amount of money in the piggy-bank is " ++ show r ++ "."

readi2_ s = C.readInt s >>= \(i1, s1) ->
  C.readInt (BU.unsafeTail s1) >>= \(i2, _) ->
  return $! (Item i2 i1)

readi2 = fromJust . readi2_
{-# INLINE readi2 #-}

readint = fst . fromJust . C.readInt

processinputs all = go (readint nt) inputs
  where (nt, rest) = C.break (== '\n') all
        (_, inputs) = C.span (== '\n') rest
        go !k s
          | k < 1 = return ()
          | otherwise = knapsack_ (w2-w1) r >> go (pred k) r3
            where (l1, r1) = C.break (== '\n') s
                  (l2, r2) = C.break (== '\n') (BU.unsafeTail r1)
                  (w1:w2:_) = map readint (C.words l1)
                  n = readint l2
                  (r, r3) = go1 n [] (BU.unsafeTail r2) 
                  go1 !n m s
                    | n < 1 = (m, s)
                    | otherwise = go1 (pred n) (it : m) (BU.unsafeTail r1)
                      where (l1, r1) = C.break (== '\n') s
                            it = readi2 l1
main = C.getContents >>= processinputs
