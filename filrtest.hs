{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Maybe

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

filrtest n s = (max (n - p) 0) `div` (l - p)  
  where !p = (\u -> (u ! (l-1))) . kmpPrefix $ s
        !l = C.length s

pr = foldr p1 ""
  where p1 x = shows x . showString "\n"

readpair s = fromJust $ C.readInt s >>= \(x, s1) ->
  return $! (x, C.tail s1)
main = C.getContents >>= putStr . pr . map (uncurry filrtest . readpair) . init . C.lines
