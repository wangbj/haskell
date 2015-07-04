{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Unsafe as BLU
import Data.ByteString.Lazy.Builder
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707
import Data.ByteString.Lazy.Builder.ASCII    
#endif
import Data.Maybe(fromJust)
import Data.Monoid
import Data.Int
import System.IO
import Data.Char

data I = I {
    _i1 :: {-# UNPACK #-} !Int
  , _i2 :: {-# UNPACK #-} !Int
  , _i3 :: {-# UNPACK #-} !Int
  } 

isqrt = ceiling . sqrt . fromIntegral
{-# INLINE isqrt #-}

venom :: Int -> Int -> Int -> Int64
venom h p a = r'
  where (h', p', a') = (fromIntegral h, fromIntegral p, fromIntegral a) :: (Int64, Int64, Int64)
        !b = 2*a'-p'
        !delta = sqrt . fromIntegral $ (b*b+(8*p'*(h'-a')))
        !r = ((fromIntegral b) + delta ) / (fromIntegral (2*p)) :: Double
        !r' = 2 * (ceiling r) - 1

{-# INLINE venom_ #-}
venom_ :: Int -> Int -> Int -> Int64
venom_ h p a = r'
  where !h' = fromIntegral h :: Int64
        !p' = fromIntegral p :: Int64
        !a' = fromIntegral a :: Int64
        !b = 2*a'-p'
        !delta2 = b*b + 8*p'*(h'-a')
        !delta = isqrt delta2
        !r = (b + delta - 1) `div` (2*p')
        !r' = 2*r+1

venom' (I h p a) = venom_ h p a
{-# INLINE venom' #-}

readj3 = C.foldl' go (1, (I 0 0 0))
  where go (!k, !i@(I x y z)) c
          | c == ' ' = (succ k, i)
          | k == 1 = (k, I (10*x+ord c - ord '0') y z)
          | k == 2 = (k, I x (10*y+ord c - ord '0') z)
          | k == 3 = (k, I x y (10*z+ord c - ord '0'))
        go :: (Int, I) -> Char -> (Int, I)
        {-# INLINE go #-}

readi3_ s = C.readInt s >>= \(!i1, !s1) ->
  (C.readInt . BLU.unsafeTail) s1 >>= \(!i2, !s2) ->
  (C.readInt . BLU.unsafeTail) s2 >>= \(!i3, _) ->
  return $! (I i1 i2 i3)
{-# INLINE readi3_ #-}
{-# INLINE readi3 #-}
{-# INLINE readj3 #-}
readi3 = fromJust . readi3_
--readi3 = snd . readj3
pr = foldr p1 ""
  where p1 x = shows x . showString "\n"
        p1 :: Int64 -> ShowS
        {-# INLINE p1 #-}
{-# INLINE pr #-}

main = L.getContents >>= processinputs

{-# INLINE process1 #-}
{-# INLINE processall #-}
{-# INLINE processinputs #-}
process1 = putStr . pr . map (venom' . readi3) . C.lines
processall [] = return ()
processall ccs@(c:[]) = process1 c
processall ccs@(c1:c2:cs) = process1 c1' >> processall (c2' : cs)
  where !c'@(!c1', c1'') = C.breakEnd (== '\n') c1
        !c2' = C.concat [c1'', c2]
processinputs = processall . L.toChunks . L.tail . snd . L.break (== '\n')
