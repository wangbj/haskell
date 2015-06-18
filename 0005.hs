{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.ByteString.Builder
import Control.Monad.Primitive
import Control.Monad
import Data.Monoid
import Data.List
import Data.Char
import System.IO (stdout)

type Palin = V.Vector Int


fromBS = V.map dig . V.fromList . C.unpack
  where dig x = ord x - ord '0'

type PalinM =  V.MVector (PrimState IO) Int

fixupM :: PalinM -> Int -> Int -> IO ()
fixupM m i j
  | i < 0 = return ()
  | i == 0 = MV.read m i >>= \vi -> when (vi > 9 && i /= j) ( MV.write m j 1 )
  | otherwise = do
    let !i' = pred i
        !j' = succ j
    vi <- MV.read m i
    vi' <- MV.read m i'
    when (vi > 9) (MV.write m i 0 >> MV.write m j 0 >> MV.write m i' (succ vi') >> MV.write m j' (succ vi'))
    fixupM m i' j'
      
nextPalinM :: PalinM -> IO ()
nextPalinM x = go x 0 (MV.length x - 1) False False
  where go m !i !j !c u
          | i >= j = when (c || not u) (MV.read m j >>= \v1 -> MV.write m j (succ v1) >> MV.write m i (succ v1) ) >> fixupM m j i
          | otherwise = do
            v1 <- MV.read m i
            v2 <- MV.read m j
            let !i' = succ i
                !j' = pred j
            case compare v1 v2 of
              EQ -> go m i' j' c u
              GT -> MV.write m j v1 >> go m i' j' False True
              LT -> MV.write m j v1 >> go m i' j' True True
        go :: PalinM -> Int -> Int -> Bool -> Bool -> IO ()

c1 = C.pack "2134"
c2 = C.pack "94187978322"
c3 = C.pack "214633"

nextPalinMut :: C.ByteString -> IO Palin
nextPalinMut c = do
  let v1 = fromBS c
  v2 <- V.unsafeThaw v1 :: IO PalinM
  nextPalinM v2
  v3 <- V.unsafeFreeze v2 :: IO Palin
  return v3

printPalin :: Palin -> IO ()
printPalin x = hPutBuilder stdout (go x 0 (V.length x))
  where go v 0 l = if (l == 1 && (v V.! 0 > 9) ) then string8 "11\n" else intDec (v V.! 0) <> go v 1 l
        go v !i l
          | i >= l = char8 '\n'
          | otherwise = intDec (v V.! i) <> go v (succ i) l

doNextPalin_ :: C.ByteString -> IO ()
doNextPalin_ s = nextPalinMut s >>= printPalin

getinputs s = C.lines rest
  where (_, t1) = C.break (== '\n') s
        (_, rest) = C.span (== '\n') t1

process = mapM_ doNextPalin_ . getinputs

main = C.getContents >>= process
