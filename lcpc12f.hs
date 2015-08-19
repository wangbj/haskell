{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.Maybe
import Data.Int

fromList = IntMap.fromListWith (+) . flip zip (repeat 1)

match :: Int -> IntMap Int -> Int64
match !x m = (\r -> r `div` 2) . IntMap.foldrWithKey go 0 $ m
  where go k v r
          | k == k' = r + (fromIntegral v)* (fromIntegral (v-1))
          | k /= k' = case IntMap.lookup k' m of
            Nothing -> r
            Just v' -> r + (fromIntegral v) * (fromIntegral v')
          where !k' = x - k
        go :: Int -> Int -> Int64 -> Int64

lcpc12f (x:_:xs) = match x (fromList xs)

process = pr . map (lcpc12f . map readint . C.words) . tail . C.lines
  where readint = fst . fromJust . C.readInt
        {-# INLINE readint #-}

pr :: [Int64] -> IO ()
pr = putStr . foldr p1 "" . zip [1..]
  where p1 (!x, !y) = shows x . showString ". " . shows y . showString "\n"

main = C.getContents >>= process
