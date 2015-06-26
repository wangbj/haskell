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

maxn = 500000

ex1 = C.pack $ unlines [
    "3"
  , "2"
  , "10"
  , "20"   ]
isqrt = floor . sqrt . fromIntegral

rmw iou p k = readArray iou k >>= \v -> writeArray iou k (v + new)
  where !d = k `quot` p
        !new = if d == p then p else d + p

roundup lo p = (lo+p-1)`div`p * p
sieve1 iou p = do
  (lo, hi) <- getBounds iou
  let !lo' = roundup lo p
      !lo'' = max (p*p) lo'
      el = [lo'', (lo''+p)..hi]
  when (not . null $ el) ( mapM_ (rmw iou p) el )

sieve iou ps = do
  (lo, hi) <- getBounds iou
  mapM_ (sieve1 iou) ps

mkAns begin end = do
  iou <- newArray (begin, end) 1 :: IO (IOUArray Int Int)
  when (begin == 1) (writeArray iou begin 0)
  sieve iou ([2..isqrt end])
  unsafeFreeze iou :: IO (UArray Int Int)

readint = fst . fromJust . C.readInt

getinputs = map readint . tail . C.lines

process inputs = do
  ans <- mkAns 1 maxn
  let ins = getinputs inputs
  mapM_ (print . (ans !)) ins

main = C.getContents >>= process
