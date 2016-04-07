{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Array.Unboxed
import Data.IntSet(IntSet)
import Data.IntMap.Strict(IntMap)
import Data.List
import Data.Function
import Data.Maybe
import Data.Ord
import Data.Monoid

squareSum 0 = 0
squareSum x = r*r + squareSum x'
  where (!x', !r) = x `quotRem` 10

sumq :: Int -> [Int]
sumq = iterate . fix $ (\f n -> if n == 0 then 0 else let (!n', !r) = n `quotRem` 10 in r*r + f n')

fromList = IntMap.fromList . flip zip [1..]

sq = reverse . snd . choose (IntSet.empty, []) . sumq
  where choose (s, q) (x:xs)
          | x `IntSet.member` s = (s, q)
          | x `IntSet.notMember` s = choose (IntSet.insert x s, x:q) xs

-- | generated from ``map (length . sq) [1..648]``, while 648 is sum square for 99999999
(wheelCache, wheelCacheSize) = (listArray (1, length wheels) wheels :: UArray Int Int, length wheels)
  where wheels = [1,9,13,8,12,17,6,13,12,2,10,13,3,14,11,8,13,11,5,8,13,14,4,9,11,10,14,4,10,13,3,4,12,12,13,16,8,10,13,9,14,8,12,5,15,12,11,14,5,12,11,11,13,15,13,10,12,8,10,17,9,10,16,12,10,15,10,3,13,6,13,14,9,11,12,10,10,12,4,13,11,4,10,14,9,3,12,15,8,12,5,10,13,5,10,13,4,9,16,2,10,13,3,14,11,9,13,11,5,10,14,18,11,12,15,11,12,16,11,13,18,13,15,14,14,15,16,14,4,3,11,15,6,11,14,13,11,12,6,14,12,14,11,13,8,14,16,12,10,11,15,14,14,9,12,11,13,13,14,9,11,15,13,14,11,10,4,11,17,13,12,16,11,16,13,4,17,13,12,11,16,14,12,12,13,11,13,5,15,5,11,4,6,10,14,17,12,15,14,9,13,14,4,9,11,10,14,4,10,13,18,13,15,14,14,15,16,14,4,14,13,14,14,10,13,6,13,15,9,4,15,14,15,11,11,6,11,11,6,9,14,10,11,17,16,11,14,15,11,11,14,13,11,16,16,11,13,14,11,10,15,6,6,11,11,11,9,15,19,14,16,13,11,14,13,9,14,13,12,4,14,15,11,15,14,15,13,16,11,10,4,9,6,11,11,19,12,11,11,13,3,4,12,12,13,16,9,10,13,3,11,15,6,11,14,13,11,12,6,4,15,14,15,11,11,6,11,11,6,12,6,15,15,13,13,16,11,5,17,12,11,11,13,15,13,10,12,9,10,13,14,11,13,13,11,7,11,10,16,16,13,6,16,10,7,12,6,6,16,9,11,11,11,12,11,6,14,14,7,10,12,11,5,9,10,6,14,12,10,13,6,6,17,10,16,16,7,10,13,9,14,9,12,5,15,12,11,14,5,14,12,14,11,13,9,14,16,12,10,9,14,10,11,17,16,11,14,15,11,12,11,11,13,15,13,10,12,9,10,5,13,17,15,15,13,4,12,14,12,15,9,16,13,13,16,11,13,12,14,12,14,11,10,4,11,16,11,12,7,11,16,14,12,12,13,11,13,5,15,14,12,15,9,14,12,12,5,14,12,5,10,11,10,12,14,7,15,12,14,12,11,11,13,15,13,10,12,9,10,11,15,14,14,9,12,11,13,13,14,11,14,13,11,16,16,11,13,14,11,13,14,11,13,13,11,7,11,10,16,15,9,16,13,13,16,11,13,12,14,13,12,16,11,16,13,4,17,13,12,10,11,11,7,11,4,5,11,15,15,12,13,13,11,13,17,11,16,13,13,9,13,14,10,12,13,15,13,15,14,10,14,11,16,14,12,15,13,14,14,17,9,10,16,12,10,15,10,3,13,9,11,15,13,14,11,10,4,11,17,10,15,6,6,11,11,11,9,15,19,16,13,6,16,10,7,12,6,6,16,12,14,11,10,4,11,16,11,12]

wheelSize x
  | x <= wheelCacheSize = wheelCache ! x
  | x > wheelCacheSize = succ . wheelSize . squareSum $ x

newtype Min a = Min {
  getMin :: a
  } deriving (Show, Eq)

instance (Ord a) => Ord (Min a) where
  compare (Min a) (Min b) = compare a b

instance (Bounded a, Ord a) => Monoid (Min a) where
  mempty = Min maxBound
  (Min x) `mappend` (Min y) = Min (min x y)

cnt :: IntMap Int -> [(Int, Int)] -> Int
cnt m = getMin' . foldl acc mempty
  where acc r (k, x) = case IntMap.lookup x m of
          Nothing -> r
          Just y -> r `mappend` (Min (k+y))
        getMin' (Min x) | x == maxBound = 0
                        | x /= maxBound = x

df x y | len1  < len2 = (x, y, cnt (fromList ys) (zip [1..] xs))
       | len1 >= len2 = (x, y, cnt (fromList xs) (zip [1..] ys))
  where xs = sq x
        ys = sq y
        len1 = wheelSize x
        len2 = wheelSize y

pr (x, y, z) = putStrLn $ show x ++ " " ++ show y ++ " " ++ show z

readi2 s = (x, y)
  where (x:y:_) = map readint (C.words s)
        readint = fst . fromJust . C.readInt

main = C.getContents >>= mapM_ (pr . uncurry df . readi2) . init . C.lines
