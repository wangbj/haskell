{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Array.Unsafe as ArrayUnsafe
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Array.Unboxed
import Data.Array.IO
import System.Random
import System.IO(stdout)
import Data.Maybe(fromJust)
import Data.Monoid
import Data.List(foldl')

swap a i j = do
  ai <- readArray a i
  aj <- readArray a j
  writeArray a j ai  
  writeArray a i aj

partition a p r = do
  x <- readArray a r
  go x (p-1) p r
  where go !x !i !j !n 
          | j >= n = swap a (i+1) r >> return (i+1)
          | otherwise = do
            aj <- readArray a j
            if aj <= x then do
              swap a (i+1) j
              go x (i+1) (j+1) n
              else go x i (j+1) n

partitionst a p r g = do
  let (i, g') = randomR (p, r) g
  swap a r i
  res <- partition a p r
  return (res, g')

quicksort a p r
  | p < r = do
    q <- partition a p r
    quicksort a p (q-1)
    quicksort a (q+1) r
  | otherwise = return ()

quicksortst a p r g
  | p < r = do
    (q, g') <- partitionst a p r g
    g'' <- quicksortst a p (q-1) g'
    quicksortst a (q+1) r g''
  | otherwise = return g

quicksort' a p r = do
  g <- getStdGen
  quicksortst a p r g
  return ()
--

readInt = fst . fromJust . C.readInt
getinputs contents =
  let (nr:inputs) = map readInt $ C.lines contents
  in (nr, inputs)

pr uarr =
  go begin end uarr
  where (begin, end) = bounds uarr
        go k n u
          | k > n = mempty
          | otherwise = (intDec (u ! k) <> char8 '\n' ) <> go (1+k) n u

tsort (nr, ints) = do
  array <- newListArray (1, nr) ints :: IO (IOUArray Int Int)
  quicksort' array 1 nr
  iarr <- ArrayUnsafe.unsafeFreeze array :: IO (UArray Int Int)
  return iarr

main = C.getContents >>= tsort . getinputs >>= hPutBuilder stdout . pr
