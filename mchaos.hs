{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -O2 -with-rtsopts=-A64m #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Maybe(fromJust)
import Data.Int
import Data.Bits
import Data.Char
import Data.List
import Text.Printf

newtype SInt = SInt Int64
             deriving (Eq, Ord)

{-
showSInt (SInt x) = go 1 (fromIntegral (x .&. 0x1f)) x
  where go k n x
          | k > n = []
          | otherwise = let !y = (x `shiftR` (5 * (10-k+1))) .&. 0x1f
                            !c = chr (fromIntegral y - 1 + ord 'a')
                        in c : go (succ k) n x

instance Show SInt where
  show = showSInt
-}

readl :: C.ByteString -> SInt
readl s = SInt r'
  where go !(!l, !sh) c = (l', sh')
          where !ci = fromIntegral (ord c - ord 'a' + 1) :: Int64
                !l' = l .|. (ci `shiftL` (sh*5))
                !sh' = pred sh
        !(!r, !z) = C.foldl go (0, 10) s
        !r' = r .|. (fromIntegral (10-z))

getinputs = map readl . tail . C.words

rev :: SInt -> SInt
rev (SInt x) = SInt ((x' `shiftL` (5*(10 - fromIntegral len))) .|. len )
  where !i9 = (x `shiftR` (10*5)) .&. 0x1f
        !i8 = (x `shiftR` (9*5)) .&. 0x1f
        !i7 = (x `shiftR` (8*5)) .&. 0x1f
        !i6 = (x `shiftR` (7*5)) .&. 0x1f
        !i5 = (x `shiftR` (6*5)) .&. 0x1f
        !i4 = (x `shiftR` (5*5)) .&. 0x1f
        !i3 = (x `shiftR` (4*5)) .&. 0x1f
        !i2 = (x `shiftR` (3*5)) .&. 0x1f
        !i1 = (x `shiftR` (2*5)) .&. 0x1f
        !i0 = (x `shiftR` (1*5)) .&. 0x1f
        !len = x .&. 0x1f
        !x' = (i0 `shiftL` (10*5)) .|.
              (i1 `shiftL` (9*5)) .|.
              (i2 `shiftL` (8*5)) .|.
              (i3 `shiftL` (7*5)) .|.
              (i4 `shiftL` (6*5)) .|.
              (i5 `shiftL` (5*5)) .|.
              (i6 `shiftL` (4*5)) .|.
              (i7 `shiftL` (3*5)) .|.
              (i8 `shiftL` (2*5)) .|.
              (i9 `shiftL` (1*5))

mchaos :: [SInt] -> Int64
mchaos s = fst $ Set.foldl go (0, s2) s1
  where !s1 = Set.fromList s
        !s2 = Set.map rev s1
        go !(!r, !t) i = let !ir = rev i
                             !j = Set.findIndex ir t
                             !t' = Set.deleteAt j t
                             !r' = r + (fromIntegral j)
                         in (r', t')

main = C.getContents >>= print . mchaos . getinputs

--
ex1 = C.pack $ unlines [
    "14"
  , "branimir"
  , "vladimir"
  , "tom"
  , "kruz"
  , "bred" 
  , "pit"
  , "zemlja"
  , "nije"
  , "ravna"
  , "ploca"
  , "ko"
  , "je"
  , "zapalio" 
  , "zito" ]  

ex2 = C.pack $ unlines [
    "2"
  , "lova"
  , "novac"  ]

ex3 = C.pack $ unlines [
    "2"
  , "aron"
  , "sunce"  ]

ex4 = C.pack $ unlines [
    "4"
  , "lova"
  , "novac"
  , "aron"
  , "sunce"  ]

ex5 = C.pack $ unlines [
    "4"
  , "lfrydanks"
  , "xtgdvjmr"
  , "cp"
  , "trhdx"   ]
