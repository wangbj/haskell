{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
import qualified Data.ByteString.Char8 as C
import Data.Maybe(fromJust)
import Control.Monad
import Data.Array.Unboxed
import Data.Array.IO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import Data.Array.Unsafe
#endif

maxn = 10^6 :: Int

primes = scanl (+) 2 [1,2,2,4,2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,14,4,6,2,10,2,6,6,4,6,6,2,10,2,4,2,12,12,4,2,4,6,2,10,6,6,6,2,6,4,2,10,14,4,2,4,14,6,10,2,4,6,8,6,6,4,6,8,4,8,10,2,10,2,6,4,6,8,4,2,4,12,8,4,8,4,6,12,2,18,6,10,6,6,2,6,10,6,6,2,6,6,4,2,12,10,2,4,6,6,2,12,4,6,8,10,8,10,8,6,6,4,8,6,4,8,4,14,10,12,2,10,2,4,2,10,14,4,2,4,14,4,2,4,20,4,8,10,8,4,6,6,14,4,6,6,8,6] :: [Int]

roundup lo p = let !r = (lo+p-1)`div`p * p in r

rmw :: IOUArray Int Int -> Int -> Int -> IO ()
rmw iou p k = readArray iou k >>= \v -> when (v == 0) (writeArray iou k p)
{-# INLINE rmw #-}
sieve iou begin end k = do
  let !k2 = k*k
      !lo = max ((begin+k-1-k2)`div`k) 0
      !hi = (end-k2)`div`k
  when (hi >= lo) (mapM_ (rmw iou k) [k2+k*x | x <- [lo..hi]])      

mkArray begin end = do
  iou <- newArray (begin, end) 0 :: IO (IOUArray Int Int)
  mapM_ (sieve iou begin end) primes
  unsafeFreeze iou :: IO (UArray Int Int)

data P = P !Int !Int
       deriving Show

{-# INLINE divrec #-}
divrec :: Int -> Int -> P
divrec !n !d = go n 1
  where go !k !r
          | rem /= 0 = P k r
          | rem == 0 = go k' (succ r)
          where !(!k', !rem) = k `quotRem` d
        go :: Int -> Int -> P
        {-# INLINE go #-}

factp :: UArray Int Int -> Int -> Bool
factp u n = go n
  where go 1 = True
        go !n = let !v = u ! n
                in if v == 0 then True else
                     let !(P n' c) = divrec n v
                         !rr = factp u n'
                     in if c > 2 then False else rr
        go :: Int -> Bool

facts' :: UArray Int Int -> Int -> Int
facts' u n = go n
  where go 1 = 1
        go !n = let !v = u ! n
                in if v == 0 then 2 else
                     let !(P n' c) = divrec n v
                         !rr = facts' u n'
                     in c * rr
        go :: Int -> Int
        {-# INLINE go #-}

div2 u = go 1 1
  where go k j
          | k > maxn = []
          | otherwise = if (u!k) == 0 then go (succ k) j else
                          if not (factp u k) then go (succ k) j else if j == 108 then k : go (succ k) 1 else
                                                                      go (succ k) (succ j)
        go :: Int -> Int -> [Int]

pr = foldr p1 ""
  where p1 x = shows x . showString "\n"
        p1 :: Int -> ShowS

main = mkArray 1 maxn >>= putStr . pr . div2
