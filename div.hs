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

--maxn = 10^6 :: Int
maxn = 999928

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

{-# INLINE divrec #-}
divrec :: Int -> Int -> P
divrec !n !d = go n 1
  where go !k !r
          | rem /= 0 = P k r
          | rem == 0 = go k' (succ r)
          where !(!k', !rem) = k `quotRem` d
        go :: Int -> Int -> P
        {-# INLINE go #-}

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

fact2 u 1 = False
fact2 u n =
  let !v1 = u ! n
  in if v1 == 0 then False else
       let !n' = n `div` v1
           !v2 = u ! n'
       in if v2 == 0 && n' /= v1 then True else False

div_ :: UArray Int Int -> [Int]
div_ s = go 2 1
  where go !n !k = if n > maxn then [] else
                     let !nf = facts' s n
                         !f2 = fact2 s nf
                     in if not f2 then go (succ n) k else
                          if k == 9 then n : go (succ n) 1
                          else go (succ n) (succ k)
        go :: Int -> Int -> [Int]
        {-# INLINE go #-}

pr = foldr p1 ""
  where p1 x = shows x . showString "\n"
        p1 :: Int -> ShowS

main = mkArray 1 maxn >>= putStr . pr . div_
