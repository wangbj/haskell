{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Foldable as F
import Data.Maybe(fromJust)
import Control.Monad
import Data.Array.Unboxed
import Data.Array.IO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import Data.Array.Unsafe
#endif
import Data.Int

maxn = 10^6

loopM_ f [] = return ()
loopM_ f (x:xs) = f x >> loopM_ f xs

primes1000 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997] :: [Int64]

{-# INLINE toggle #-}
toggle iou k = writeArray iou k False
sieveprimes iou begin end k = do
  let !k2 = k*k
      lo = max ((begin+k-1-k2)`div`k) 0
      hi = (end-k2)`div`k
  when (hi >= lo) (loopM_ (toggle iou) [k2+k*x | x <- [lo..hi]])

genprimes begin end = do
  iou <- newArray (begin, end) True :: IO (IOUArray Int64 Bool)
  when (begin == 1) (writeArray iou 1 False)
  loopM_ (sieveprimes iou begin end) primes1000
  u <- unsafeFreeze iou :: IO (UArray Int64 Bool)
  return $! u

{-# INLINE rmw #-}
rmw iou k = readArray iou k >>= \v -> writeArray iou k (1+v)
loopA_ f u = go f u lo
  where (lo, hi) = bounds u
        go f u k
          | k > hi = return ()
          | otherwise = when (u ! k) (f k) >> go f u (succ k)

sievefacts :: IOUArray Int64 Int64 -> Int64 -> Int64 -> Int64 -> IO ()
sievefacts iou begin end k = loopM_ (rmw iou) [k, k*2 .. end]
gensieved  begin end = do
  iou <- newArray (begin, end) 0 :: IO (IOUArray Int64 Int64)
  primes <- genprimes begin end
  loopA_ (sievefacts iou begin end) primes
  u <- unsafeFreeze iou :: IO (UArray Int64 Int64)
  return u

primes u = go hi []
  where (lo, hi) = bounds u
        go !k xs
          | k < lo = xs
          | otherwise = if (u ! k) then go (pred k) (k:xs) else go (pred k) xs

divrec x d = go x d 0
  where go !x !d !c
          | r == 0  = go x' d (succ c)
          | otherwise = (c, x)
          where (x', r) = x `quotRem` d

twosqrsHelper p x = go p x
  where go xxs@(x:xs) t
          | x*x > t = if t `mod` 4 == 3 then False else True
          | even c = go xs t'
          | odd c = if x `mod` 4 == 3 then False else go xs t'
          where (c, t') = divrec t x

pr False = "No"
pr True = "Yes"

twosqrs :: [Int64] -> IO ()
twosqrs xxs = do
  p1m <- genprimes 1 (10^6)
  let p = primes p1m
  mapM_ (putStrLn . pr .  twosqrsHelper p) xxs

readint64 = fromIntegral . fst . fromJust . C.readInteger
getinputs = map readint64 . tail . C.words

main = C.getContents >>= twosqrs . getinputs
