{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Int

data P = P {
    _p_x :: {-# UNPACK #-} !Int
  , _p_y :: {-# UNPACK #-} !Int
  }

readP2 s = fromJust $ C.readInt s >>= \(x, s1) ->
  (C.readInt . C.tail ) s1 >>= \(y, _) ->
  return $! P x y
  
bf n k = ((n^(1+k)-1) `div` (n-1) - 1) `mod` 1000000007

-- calculate a power with respect to a modulus, first tests and
-- forwarding to another helper
powerWithModulus :: Int -> Int -> Int -> Int
powerWithModulus mo n k
   | mo < 0     = powerWithModulus (-mo) n k
   | k == 0     = 1
   | k == 1     = n `mod` mo
   | odd k      = powerMod mo n n (k-1)
   | otherwise  = powerMod mo 1 n k

mulmod :: Int -> Int -> Int -> Int
mulmod x y m = r
  where !x' = fromIntegral x :: Int64
        !y' = fromIntegral y :: Int64
        !m' = fromIntegral m :: Int64
        !p = x' * y' `mod` m'
        !r = fromIntegral p
{-# INLINE mulmod #-}

-- calculate the power with auxiliary value to account for
-- odd exponents on the way, we have @mo >= 2@ and @k >= 1@
powerMod :: Int -> Int -> Int -> Int -> Int
powerMod !mo !aux !val !k
  | k == 1 = mulmod aux val mo
  | odd k  = powerMod mo (mulmod aux val mo) (mulmod val val mo) (k `div` 2)
  | even k = powerMod mo aux (mulmod val val mo)  (k `div` 2)


ucv (P n k) = r5
  where !r1 = powerWithModulus p n (1+k)
        !r2 = ((r1 `mod` p) + (p-1) ) `mod` p
        !r3 = powerWithModulus p (n-1) (p-2)
        !r4 = mulmod r2 r3 p
        !r5 = (r4+(p-1)) `mod` p
        p = 10^9 + 7

pr = putStr . foldr p1 ""
  where p1 x = shows x . showString "\n"

main = C.getContents >>= pr . map (ucv . readP2) . init . C.lines
