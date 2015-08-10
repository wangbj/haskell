{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Monoid
import System.IO

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
epalin s = (s, C.drop p t)
  where !t = C.reverse s
        !p = kmpMatch s t

pr = foldr (uncurry p1) mempty
  where p1 s e r = byteString s <> byteString e <> char7 '\n' <> r

main = C.getContents >>= hPutBuilder stdout . pr . map epalin . C.lines
