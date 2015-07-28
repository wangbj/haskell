import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU
import Data.Bits
import Data.Maybe(fromJust)
import Data.Int

data BC = BC {
    __n :: {-# UNPACK #-} !Int
  , __m :: {-# UNPACK #-} !Int
  , __k :: {-# UNPACK #-} !Int
  } deriving Show

data R = R {
    _r1 :: {-# UNPACK #-} !Int64
  , _r2 :: {-# UNPACK #-} !Int
  } deriving Show

bc1 n m k =  (k'-1) + k'*(n'-1 + (n')*(m'-1))
  where n' = fromIntegral n
        m' = fromIntegral m
        k' = fromIntegral k

bc2 n m k = ilog n + ilog m + ilog k

bc n m k = R (bc1 n m k) (bc2 n m k)
bc' (BC n m k) = bc n m k
{-# INLINE bc' #-}

ilogR :: Int -> Int
ilogR = go 0
  where go r x
          | x == 1 = r
          | otherwise = go (succ r) (x `shiftR` 1)
{-# INLINE ilogR #-}

ilog n
  | n .&. (n-1) == 0 = r
  | otherwise = succ r
  where r = ilogR n

readbc_ s = C.readInt s >>= \(n, s1) ->
  (C.readInt . BU.unsafeTail $ s1) >>= \(m, s2) ->
  (C.readInt . BU.unsafeTail $ s2) >>= \(k, _) ->
  return $! BC n m  k
{-# INLINE readbc_ #-}

readbc = fromJust . readbc_

getinputs = map readbc . tail . C.lines


pr s = go (zip [1..] s)
  where go = foldr p1 ""
          where p1 (k, (R x y)) = showString "Case #" . shows k . showString ": " . shows x . showString " " . shows y . showString "\n"

process = putStr . pr . map bc' . getinputs

main = C.getContents >>= process
