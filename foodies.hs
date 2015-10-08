{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.List
import Control.Monad
import Data.Array.Unboxed
import Data.Array.IO
import Data.Array.Unsafe
import Data.Char
import Data.Int
import Data.Maybe

maxn = 100000 :: Int

build' iou n s = go n 0 s
  where go k p [] = return ()
        go k p (x:xs) = forM_ [(p+1)..x] (
          \i -> writeArray iou i k) >> go (pred k) x xs
          
build n s = do
  iou <- newArray (1, maxn) 0 :: IO (IOUArray Int Int)
  build' iou n s
  u <- unsafeFreeze iou :: IO (UArray Int Int)
  return u

consume :: UArray Int Int -> Int64 -> Int64
consume u n = go 0 hi n
  where (!lo, !hi) = bounds u
        go !r !k !x
          | x <= 0 || k < lo = r
          | otherwise = let !v = fromIntegral (u ! k)
                        in if v > x then (r+(fromIntegral k)*(fromIntegral x))
                        else go (r+(fromIntegral v)*(fromIntegral k)) (pred k) (x-(fromIntegral v))
        go :: Int64 -> Int -> Int64 -> Int64
fd n s a = build n (sort s) >>= \u -> return $! consume u a


readint :: C.ByteString -> Int
readint64 :: C.ByteString -> Int64

readint = fst . fromJust . C.readInt
readint64 = fromIntegral . fst . fromJust . C.readInteger

processinputs 0 _ = return ()
processinputs n (l1:l2:l3:rest) = fd nints ints x >>= print >> processinputs (pred n) rest
  where !nints = readint l1
        !ints = map readint (C.words l2)
        !x = readint64 l3

processall inputs = processinputs (readint l1) lines
  where (l1:lines) = C.lines inputs

main = C.getContents >>= processall
