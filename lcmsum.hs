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
import Data.Int

maxn = 10^6

loopM_ f k s
  | k > maxn = return ()
  | otherwise = f k >> loopM_ f (k+s) s

f2 :: IOUArray Int Int -> Int -> IO ()
f2 u i = readArray u i >>= \v ->
  when (v == fromIntegral i) ( do
                               let go g k s = when (k <= maxn) (
                                     g k >> go g (s+k) s )
                                 in go f21 (2*i) i )
  where f21 j = readArray u j >>= \v1 ->
          writeArray u j (v1 - (v1 `div` (fromIntegral i)) )

f3 :: UArray Int Int -> IOUArray Int Int64 -> Int -> IO ()
f3 u r i = let !v = u ! i
            in go (f31 v) i i
  where f31 v j = readArray r j >>= \v1 ->
          writeArray r j (v1 + (fromIntegral i) * (fromIntegral v))
        f31 :: Int -> Int -> IO ()
        go g k s = when (k <= maxn) (
          g k >> go g (s+k) s )

mkArr = do
  iou <- newArray_ (1, maxn) :: IO (IOUArray Int Int)
  res <- newArray_ (1, maxn) :: IO (IOUArray Int Int64)
  loopM_ ((\u k -> writeArray u k k) iou) 1 1
  loopM_ (f2 iou) 2 1

  loopM_ ( (\u i -> readArray u i >>= \v -> when (v == i) (writeArray u i (i-1)) ) iou ) 2 1

  uu <- unsafeFreeze iou :: IO (UArray Int Int)
  loopM_ (f3 uu res) 1 1

  unsafeFreeze res :: IO (UArray Int Int64)

lcmsum :: UArray Int Int64 -> Int -> Int64
lcmsum u n = (1 + (u ! n)) * (fromIntegral n) `div` 2
getinputs = map readint . tail . C.lines
  where readint = fst . fromJust . C.readInt

pr = foldr p1 ""
  where p1 x = shows x . showString "\n"
        
process inputs = mkArr >>= \u ->
  putStr . pr . map (lcmsum u) . getinputs $ inputs

main = C.getContents >>= process
