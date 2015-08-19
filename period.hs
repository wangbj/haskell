{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Monoid
import Data.Maybe
import System.IO(stdout)

kmpPrefix_ s l u = go 0 1
  where go !i !j
          | j > l = return ()
          | s `C.index` i == s `C.index` j = writeArray u j (succ i) >> go (succ i) (succ j)
          | s `C.index` i /= s `C.index` j = if i == 0 then writeArray u j 0 >> go 0 (succ j) else
                                               readArray u (i-1) >>= \i' -> go i' j
        {-# INLINE go #-}

kmpPrefix :: C.ByteString -> UArray Int Int
kmpPrefix w = runSTUArray $ do
  let !len = C.length w - 1
  iou <- newArray (0, len) 0
  kmpPrefix_ w len iou
  return $! iou

kmpMatch s m = go 0 0
  where u = kmpPrefix m
        ls = C.length s
        lm = C.length m
        go i j
          | i >= ls = j
          | j >= lm = j
          | s `C.index` i == m `C.index` j = go (succ i) (succ j)
          | s `C.index` i /= m `C.index` j = if j == 0 then go (succ i) j else 
                                               go i (u ! (pred j))
periodHelper :: Int -> UArray Int Int -> Int
periodHelper l pfx = succ r
  where !p = (\u -> u ! (l-1)) pfx
        !r = if (p `mod` (l-p) == 0) then p `div` (l-p) else 0

period s = go 2
  where !pfx = kmpPrefix s
        !len = C.length s
        go k
          | k > len = []
          | otherwise = case periodHelper k pfx of
            0 -> go (succ k)
            1 -> go (succ k)
            rpt -> (k, rpt) : go (succ k)

process k n s
  | k > n = return ()
  | otherwise =
    let (_:s':rest) = s
    in pr k (period s') >> process (succ k) n rest
                
processall (k:ints) = process 1 (readint k) ints
  where readint = fst . fromJust . C.readInt

pr k s = hPutBuilder stdout output
  where output = string8 "Test case #" <> intDec k <> char8 '\n' <> foldl go mempty s <> char8 '\n'
          where go r (x, y) = r <> intDec x <> char8 ' ' <> intDec y <> char8 '\n'
  
main = C.getContents >>= processall . C.lines
