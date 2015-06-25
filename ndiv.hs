{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C
import qualified Data.Foldable as F
import Data.Maybe(fromJust)
import Control.Monad
import Data.Array.Unboxed
import Data.Array.IO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import Data.Array.Unsafe
#endif

isqrt = floor . sqrt . fromIntegral

inc iou k = readArray iou k >>= \v -> writeArray iou k (succ v)

roundup lo p = (lo+p-1)`div`p * p
sieve1 iou p = do
  (lo, hi) <- getBounds iou
  let !lo' = roundup lo p
      !lo'' = max (p*p) lo'
      el = [lo'', (lo''+p)..hi]
  when (not . null $ el) ( mapM_ (inc iou) el )

sieve iou ps = do
  (lo, hi) <- getBounds iou
  mapM_ (sieve1 iou) ps

initArray begin end = do
  iou <- newArray (begin, end) 0 :: IO (IOUArray Int Int)
  sieve iou ([2..isqrt end])
  u <- unsafeFreeze iou :: IO (UArray Int Int)
  return u

perfectSquare x = r * r == x
  where !r = isqrt x

ndiv_ _ begin _ 1 = if begin == 1 then 1 else 0
ndiv_ u begin end n = go begin (n-2)
  where go k d
          | k > end = 0
          | otherwise = let v = u ! k
                        in let c = if perfectSquare k then 2*v-1 else 2*v
                           in if c == d then 1 + go (succ k) d else go (succ k) d

readint = fst . fromJust . C.readInt

getinputs inputs = (begin, end, n)
  where (begin:end:n:_) = map readint (C.words inputs)

process inputs = do
  let (begin, end, n) = getinputs inputs
  u <- initArray begin end
  print $ ndiv_ u begin end n

main = C.getContents >>= process
