{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Builder
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707
import Data.ByteString.Lazy.Builder.ASCII    
#endif
import Data.Array.Unboxed
import Data.Array.IO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import Data.Array.Unsafe    
#endif
import Control.Monad
import Data.Monoid
import Data.List
import Data.Char
import System.IO (stdout)

type Palin = UArray Int Int

fromBS :: C.ByteString -> UArray Int Int
fromBS s = amap dig $ listArray (0, l-1) (C.unpack s)
  where dig x = ord x - ord '0'
        l = C.length s

type PalinM =  IOUArray Int Int

arraySizeM iou = getBounds iou >>= \(i, j) -> return $! j - i + 1
arraySize u = j - i + 1
  where (i, j) = bounds u

fixupM :: PalinM -> Int -> Int -> IO ()
fixupM m i j
  | i < 0 = return ()
  | i == 0 = readArray m i >>= \vi -> when (vi > 9 && i /= j) ( writeArray m j 1 )
  | otherwise = do
    let !i' = pred i
        !j' = succ j
    vi <- readArray m i
    vi' <- readArray m i'
    when (vi > 9) (writeArray m i 0 >> writeArray m j 0 >> writeArray m i' (succ vi') >> writeArray m j' (succ vi'))
    fixupM m i' j'

nextPalinM :: PalinM -> IO ()
nextPalinM x = arraySizeM x >>= \len -> go x 0 (len-1) False False
  where go m !i !j !c u
          | i >= j = when (c || not u) (readArray m j >>= \v1 -> writeArray m j (succ v1) >> writeArray m i (succ v1) ) >> fixupM m j i
          | otherwise = do
            v1 <- readArray m i
            v2 <- readArray m j
            let !i' = succ i
                !j' = pred j
            case compare v1 v2 of
              EQ -> go m i' j' c u
              GT -> writeArray m j v1 >> go m i' j' False True
              LT -> writeArray m j v1 >> go m i' j' True True
        go :: PalinM -> Int -> Int -> Bool -> Bool -> IO ()

c1 = C.pack "2134"
c2 = C.pack "94187978322"
c3 = C.pack "214633"

nextPalinMut :: C.ByteString -> IO Palin
nextPalinMut c = do
  let v1 = fromBS c
  v2 <- unsafeThaw v1 :: IO PalinM
  nextPalinM v2
  v3 <- unsafeFreeze v2 :: IO Palin
  return v3

printPalin :: Palin -> IO ()
printPalin x = hPutBuilder stdout (go x 0 (len))
  where go v 0 l = if (l == 1 && (v ! 0 > 9) ) then string8 "11\n" else intDec (v ! 0) <> go v 1 l
        go v !i l
          | i >= l = char8 '\n'
          | otherwise = intDec (v ! i) <> go v (succ i) l
        len = arraySize x

doNextPalin_ :: C.ByteString -> IO ()
doNextPalin_ s = nextPalinMut s >>= printPalin

getinputs s = C.lines rest
  where (_, t1) = C.break (== '\n') s
        (_, rest) = C.span (== '\n') t1

process = mapM_ doNextPalin_ . getinputs

main = C.getContents >>= process
