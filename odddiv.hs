{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet as Set
import qualified Data.IntMap as IntMap
import Data.Maybe(fromJust)
import Data.Int
import Data.Char
import Data.Array.Unboxed
import Data.Array.IO
import System.IO.Unsafe(unsafePerformIO)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import Data.Array.Unsafe
#endif

-- upper range, upto maxn^2
maxn = 10^5 :: Int

slowilogb :: Int -> Int -> Int
slowilogb b n = floor $ logBase b' n'
  where b' = fromIntegral b
        n' = fromIntegral n

fastilogbase b n
  | n /= (10^5) = slowilogb b n -- should remove this, for validation purpose.
  | otherwise = case IntMap.lookup b ilogans of
    Just !k -> k
    _ -> 1
  where ilogans = IntMap.fromList (zip p100k [16,10,7,5,4,4,4,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]) :: IntMap.IntMap Int    

{-# INLINE ilogb #-}
ilogb b n = fastilogbase b n

-- generate ndivisors for k^n
genseqHelper k = zip (map (k^) [1..end]) [3,5..]
  where !end = ilogb k maxn
-- generate ndivisors for a*b^n
genseq1 iou (k, v) = do
  mapM_ (uncurry (writeArray iou)) (zip [k, 2*k..maxn] (repeat v))
genseq iou k = do
  mapM_ (genseq1 iou) (genseqHelper k)
  unsafeFreeze iou :: IO (UArray Int Int)

sieve2 iou work k = readArray work k >>= \v -> readArray iou k >>= \u -> writeArray iou k (u*v)
sieveodiv1 iou work k = do
  genseq work k
  mapM_ (sieve2 iou work) [k, 2*k..(maxn)]
sieveodiv = do
  iou <- newArray (1, maxn) 1 :: IO (IOUArray Int Int)
  worker <- newArray (1, maxn) 0 :: IO (IOUArray Int Int)
  mapM_ (sieveodiv1 iou worker) p100k
  unsafeFreeze iou :: IO (UArray Int Int)

goodQueries = [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,39,45,49,51,55,57,63,65,69,75,77,81,85,87,91,93,95,99,105,115,117,119,121,125,133,135,143,147,153,161,165,169,171,175,187,189,195,207,225,231,243,245,255,273,275,285,297,315,325,343,345,351,357,363,375,385,399,405,425,429,441,455,459,495,513,525,567,585,625,675,693,729,735,765,819,825,875,891,945,1053,1125,1215,1323] :: [Int]

goodQueriesAns = [1,9592,65,14,23320,4,3,5626,2,2,2261,1,94,20985,1,1,537,71,279,9534,13,81,25,45,3563,16,14,1154,10,8589,8,2,6,1,6,715,827,2,336,4,2,34,2,4994,2,137,73,1,170,1,33,37,1,1609,84,5,939,52,1490,13,19,23,9,9,234,559,6,2,1,93,5,3,68,6,2,793,1,1,69,3,10,69,3,50,181,20,1,125,10,61,8,1,2,4,1,10,32,2,3,10,2] :: [Int]

goodQueriesAHelper = do
  iou <- newArray (1, last goodQueries) 0 :: IO (IOUArray Int Int)
  mapM_ (uncurry (writeArray iou)) (zip goodQueries [1..])
  unsafeFreeze iou :: IO (UArray Int Int)

goodQueriesA = unsafePerformIO  goodQueriesAHelper

answerableQuery k
  | k < 1 || k > 1323 = False
  | otherwise = (goodQueriesA ! k) /= 0
lookupQueryIndex = (goodQueriesA !)

-- | build answers array
ansA = unsafePerformIO sieveodiv

lookupArrayIndex !k = (amap ! k)
  where amap = listArray (1, last goodQueries) [1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,10,0,11,0,12,0,13,0,14,0,15,0,16,0,17,0,18,0,0,0,19,0,0,0,0,0,20,0,0,0,21,0,22,0,0,0,23,0,24,0,0,0,0,0,25,0,26,0,0,0,27,0,0,0,0,0,28,0,29,0,0,0,30,0,0,0,31,0,32,0,0,0,33,0,34,0,35,0,0,0,36,0,0,0,0,0,37,0,0,0,0,0,0,0,0,0,38,0,39,0,40,0,41,0,0,0,42,0,0,0,0,0,0,0,43,0,44,0,0,0,0,0,0,0,45,0,0,0,46,0,0,0,0,0,47,0,0,0,0,0,0,0,48,0,0,0,49,0,0,0,50,0,51,0,0,0,52,0,0,0,0,0,0,0,0,0,0,0,53,0,54,0,0,0,0,0,55,0,0,0,0,0,0,0,0,0,0,0,56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57,0,0,0,0,0,58,0,0,0,0,0,0,0,0,0,0,0,59,0,60,0,0,0,0,0,0,0,0,0,61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,0,63,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,66,0,0,0,0,0,0,0,0,0,67,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,68,0,69,0,0,0,0,0,70,0,0,0,0,0,71,0,0,0,0,0,72,0,0,0,0,0,0,0,0,0,0,0,73,0,0,0,0,0,0,0,0,0,74,0,0,0,0,0,0,0,0,0,0,0,0,0,75,0,0,0,0,0,76,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,77,0,0,0,78,0,0,0,0,0,0,0,0,0,0,0,79,0,0,0,0,0,0,0,0,0,0,0,0,0,80,0,0,0,81,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,82,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,83,0,0,0,0,0,0,0,0,0,0,0,84,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,85,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,86,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,87,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,89,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,90,0,0,0,0,0,91,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,92,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,0,0,0,0,94,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,95,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,98,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,101] :: UArray Int Int

{-# INLINE list2UArray #-}
list2UArray s = listArray (1, length s) s :: UArray Int Int
iter u a !k
  | k < 1 = return ()
  | otherwise = let !v = a ! k
                    !i = lookupArrayIndex v
                in readArray u i >>= \s -> writeArray u i (k:s) >> iter u a (pred k)

mkAks' = do
  initu <- newArray (1, length goodQueries) [] :: IO (IOArray Int [Int])
  iter initu ansA maxn
  fmap (amap list2UArray) . unsafeFreeze $ initu :: IO (Array Int (UArray Int Int))

mkAks = unsafePerformIO mkAks'

getAk = (mkAks!) . lookupQueryIndex

binarysearch u !lo !hi !k
  | lo > hi = Left lo
  | otherwise =
    case compare (u ! mid) k of
      EQ -> Right mid
      LT -> binarysearch u (succ mid) hi k
      GT -> binarysearch u lo (pred mid) k
      where !mid = (hi + lo) `div` 2

nitems u !k !begin !end = case (l, r) of
    (Left l1, Left r1) -> r1 - l1
    (Left l1, Right r1) -> r1 - l1 + 1
    (Right l1, Left r1) -> r1 - l1
    (Right l1, Right r1) -> r1 - l1 + 1
  where (lo, hi) = bounds u
        l = binarysearch u lo hi begin
        r = binarysearch u lo hi end

odddivHelper :: Int -> Int -> Int -> Int
odddivHelper !k !begin !end = nitems u k begin end
  where u = getAk k

isqrt :: Int64 -> Int
isqrt' :: Int64 -> Int
isqrt = fromIntegral . floor . sqrt . fromIntegral
isqrt' = fromIntegral . ceiling . sqrt . fromIntegral

odddiv :: Int -> Int64 -> Int64 -> Int
odddiv 1 1 _ = 1
odddiv 1 _ _ = 0
odddiv k !begin !end
  | (not . answerableQuery) k = 0
  | otherwise = odddivHelper k (isqrt' begin) (isqrt end)

odddiv' (Q k b e) = odddiv k b e

readint :: C.ByteString -> Int
readint64 :: C.ByteString -> Int64
readint = fst . fromJust . C.readInt
readint64 = fromIntegral . fst . fromJust . C.readInteger

data Q = Q {
    _q_x :: {-# UNPACK #-} !Int
  , _q_y :: {-# UNPACK #-} !Int64
  , _q_z :: {-# UNPACK #-} !Int64
  }

readQ s = Q (fromIntegral x) y z
  where acc !(!x, !y, !z, !i) !c
          | c == ' ' = (x, y, z, (succ i))
          | i == 1 = (x*10+d, y, z, i)
          | i == 2 = (x, y*10+d, z, i)
          | i == 3 = (x, y, z*10+d, i)
          where !d = fromIntegral (ord c - ord '0') :: Int64
        !(!x, !y, !z, _) = C.foldl acc (0,0,0, 1) s
        
readQ' s = Q (fromIntegral a) b c
  where (a:b:c:_) = map readint64 (C.words s)

getinputs = map readQ . (tail . C.lines)

unline c = (l1, rest)
  where (l1, r1) = C.break (== '\n') c
        (_, rest) = C.span (== '\n') r1

{-# INLINE process1 #-}
process1 = putStr . pr . map (odddiv' . readQ') . C.lines
processall [] = return ()
processall ccs@(c:[]) = process1 c
processall ccs@(c1:c2:cs) = process1 c1' >> processall (c2' : cs)
  where c'@(c1', c1'') = C.breakEnd (== '\n') c1
        c2' = C.concat [c1'', c2]

processinputs all = processall (c':cs)
  where (c:cs) = L.toChunks all
        (_, c') = unline c

pr :: [Int] -> String
pr = foldr p1 ""
  where p1 x = shows x . showString "\n"

process = putStr . pr . map odddiv' . getinputs
main = L.getContents >>= processinputs

--
p100kLength = length p100k -- should optmize to a constant automatically
p100k = scanl (+) 2 [1,2,2,4,2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,14,4,6,2,10,2,6,6,4,6,6,2,10,2,4,2,12,12,4,2,4,6,2,10,6,6,6,2,6,4,2,10,14,4,2,4,14,6,10,2,4,6,8,6,6,4,6,8,4,8,10,2,10,2,6,4,6,8,4,2,4,12,8,4,8,4,6,12,2,18,6,10,6,6,2,6,10,6,6,2,6,6,4,2,12,10,2,4,6,6,2,12,4,6,8,10,8,10,8,6,6,4,8,6,4,8,4,14,10,12,2,10,2,4,2,10,14,4,2,4,14,4,2,4,20,4,8,10,8,4,6,6,14,4,6,6,8,6,12,4,6,2,10,2,6,10,2,10,2,6,18,4,2,4,6,6,8,6,6,22,2,10,8,10,6,6,8,12,4,6,6,2,6,12,10,18,2,4,6,2,6,4,2,4,12,2,6,34,6,6,8,18,10,14,4,2,4,6,8,4,2,6,12,10,2,4,2,4,6,12,12,8,12,6,4,6,8,4,8,4,14,4,6,2,4,6,2,6,10,20,6,4,2,24,4,2,10,12,2,10,8,6,6,6,18,6,4,2,12,10,12,8,16,14,6,4,2,4,2,10,12,6,6,18,2,16,2,22,6,8,6,4,2,4,8,6,10,2,10,14,10,6,12,2,4,2,10,12,2,16,2,6,4,2,10,8,18,24,4,6,8,16,2,4,8,16,2,4,8,6,6,4,12,2,22,6,2,6,4,6,14,6,4,2,6,4,6,12,6,6,14,4,6,12,8,6,4,26,18,10,8,4,6,2,6,22,12,2,16,8,4,12,14,10,2,4,8,6,6,4,2,4,6,8,4,2,6,10,2,10,8,4,14,10,12,2,6,4,2,16,14,4,6,8,6,4,18,8,10,6,6,8,10,12,14,4,6,6,2,28,2,10,8,4,14,4,8,12,6,12,4,6,20,10,2,16,26,4,2,12,6,4,12,6,8,4,8,22,2,4,2,12,28,2,6,6,6,4,6,2,12,4,12,2,10,2,16,2,16,6,20,16,8,4,2,4,2,22,8,12,6,10,2,4,6,2,6,10,2,12,10,2,10,14,6,4,6,8,6,6,16,12,2,4,14,6,4,8,10,8,6,6,22,6,2,10,14,4,6,18,2,10,14,4,2,10,14,4,8,18,4,6,2,4,6,2,12,4,20,22,12,2,4,6,6,2,6,22,2,6,16,6,12,2,6,12,16,2,4,6,14,4,2,18,24,10,6,2,10,2,10,2,10,6,2,10,2,10,6,8,30,10,2,10,8,6,10,18,6,12,12,2,18,6,4,6,6,18,2,10,14,6,4,2,4,24,2,12,6,16,8,6,6,18,16,2,4,6,2,6,6,10,6,12,12,18,2,6,4,18,8,24,4,2,4,6,2,12,4,14,30,10,6,12,14,6,10,12,2,4,6,8,6,10,2,4,14,6,6,4,6,2,10,2,16,12,8,18,4,6,12,2,6,6,6,28,6,14,4,8,10,8,12,18,4,2,4,24,12,6,2,16,6,6,14,10,14,4,30,6,6,6,8,6,4,2,12,6,4,2,6,22,6,2,4,18,2,4,12,2,6,4,26,6,6,4,8,10,32,16,2,6,4,2,4,2,10,14,6,4,8,10,6,20,4,2,6,30,4,8,10,6,6,8,6,12,4,6,2,6,4,6,2,10,2,16,6,20,4,12,14,28,6,20,4,18,8,6,4,6,14,6,6,10,2,10,12,8,10,2,10,8,12,10,24,2,4,8,6,4,8,18,10,6,6,2,6,10,12,2,10,6,6,6,8,6,10,6,2,6,6,6,10,8,24,6,22,2,18,4,8,10,30,8,18,4,2,10,6,2,6,4,18,8,12,18,16,6,2,12,6,10,2,10,2,6,10,14,4,24,2,16,2,10,2,10,20,4,2,4,8,16,6,6,2,12,16,8,4,6,30,2,10,2,6,4,6,6,8,6,4,12,6,8,12,4,14,12,10,24,6,12,6,2,22,8,18,10,6,14,4,2,6,10,8,6,4,6,30,14,10,2,12,10,2,16,2,18,24,18,6,16,18,6,2,18,4,6,2,10,8,10,6,6,8,4,6,2,10,2,12,4,6,6,2,12,4,14,18,4,6,20,4,8,6,4,8,4,14,6,4,14,12,4,2,30,4,24,6,6,12,12,14,6,4,2,4,18,6,12,8,6,4,12,2,12,30,16,2,6,22,14,6,10,12,6,2,4,8,10,6,6,24,14,6,4,8,12,18,10,2,10,2,4,6,20,6,4,14,4,2,4,14,6,12,24,10,6,8,10,2,30,4,6,2,12,4,14,6,34,12,8,6,10,2,4,20,10,8,16,2,10,14,4,2,12,6,16,6,8,4,8,4,6,8,6,6,12,6,4,6,6,8,18,4,20,4,12,2,10,6,2,10,12,2,4,20,6,30,6,4,8,10,12,6,2,28,2,6,4,2,16,12,2,6,10,8,24,12,6,18,6,4,14,6,4,12,8,6,12,4,6,12,6,12,2,16,20,4,2,10,18,8,4,14,4,2,6,22,6,14,6,6,10,6,2,10,2,4,2,22,2,4,6,6,12,6,14,10,12,6,8,4,36,14,12,6,4,6,2,12,6,12,16,2,10,8,22,2,12,6,4,6,18,2,12,6,4,12,8,6,12,4,6,12,6,2,12,12,4,14,6,16,6,2,10,8,18,6,34,2,28,2,22,6,2,10,12,2,6,4,8,22,6,2,10,8,4,6,8,4,12,18,12,20,4,6,6,8,4,2,16,12,2,10,8,10,2,4,6,14,12,22,8,28,2,4,20,4,2,4,14,10,12,2,12,16,2,28,8,22,8,4,6,6,14,4,8,12,6,6,4,20,4,18,2,12,6,4,6,14,18,10,8,10,32,6,10,6,6,2,6,16,6,2,12,6,28,2,10,8,16,6,8,6,10,24,20,10,2,10,2,12,4,6,20,4,2,12,18,10,2,10,2,4,20,16,26,4,8,6,4,12,6,8,12,12,6,4,8,22,2,16,14,10,6,12,12,14,6,4,20,4,12,6,2,6,6,16,8,22,2,28,8,6,4,20,4,12,24,20,4,8,10,2,16,2,12,12,34,2,4,6,12,6,6,8,6,4,2,6,24,4,20,10,6,6,14,4,6,6,2,12,6,10,2,10,6,20,4,26,4,2,6,22,2,24,4,6,2,4,6,24,6,8,4,2,34,6,8,16,12,2,10,2,10,6,8,4,8,12,22,6,14,4,26,4,2,12,10,8,4,8,12,4,14,6,16,6,8,4,6,6,8,6,10,12,2,6,6,16,8,6,6,12,10,2,6,18,4,6,6,6,12,18,8,6,10,8,18,4,14,6,18,10,8,10,12,2,6,12,12,36,4,6,8,4,6,2,4,18,12,6,8,6,6,4,18,2,4,2,24,4,6,6,14,30,6,4,6,12,6,20,4,8,4,8,6,6,4,30,2,10,12,8,10,8,24,6,12,4,14,4,6,2,28,14,16,2,12,6,4,20,10,6,6,6,8,10,12,14,10,14,16,14,10,14,6,16,6,8,6,16,20,10,2,6,4,2,4,12,2,10,2,6,22,6,2,4,18,8,10,8,22,2,10,18,14,4,2,4,18,2,4,6,8,10,2,30,4,30,2,10,2,18,4,18,6,14,10,2,4,20,36,6,4,6,14,4,20,10,14,22,6,2,30,12,10,18,2,4,14,6,22,18,2,12,6,4,8,4,8,6,10,2,12,18,10,14,16,14,4,6,6,2,6,4,2,28,2,28,6,2,4,6,14,4,12,14,16,14,4,6,8,6,4,6,6,6,8,4,8,4,14,16,8,6,4,12,8,16,2,10,8,4,6,26,6,10,8,4,6,12,14,30,4,14,22,8,12,4,6,8,10,6,14,10,6,2,10,12,12,14,6,6,18,10,6,8,18,4,6,2,6,10,2,10,8,6,6,10,2,18,10,2,12,4,6,8,10,12,14,12,4,8,10,6,6,20,4,14,16,14,10,8,10,12,2,18,6,12,10,12,2,4,2,12,6,4,8,4,44,4,2,4,2,10,12,6,6,14,4,6,6,6,8,6,36,18,4,6,2,12,6,6,6,4,14,22,12,2,18,10,6,26,24,4,2,4,2,4,14,4,6,6,8,16,12,2,42,4,2,4,24,6,6,2,18,4,14,6,28,18,14,6,10,12,2,6,12,30,6,4,6,6,14,4,2,24,4,6,6,26,10,18,6,8,6,6,30,4,12,12,2,16,2,6,4,12,18,2,6,4,26,12,6,12,4,24,24,12,6,2,12,28,8,4,6,12,2,18,6,4,6,6,20,16,2,6,6,18,10,6,2,4,8,6,6,24,16,6,8,10,6,14,22,8,16,6,2,12,4,2,22,8,18,34,2,6,18,4,6,6,8,10,8,18,6,4,2,4,8,16,2,12,12,6,18,4,6,6,6,2,6,12,10,20,12,18,4,6,2,16,2,10,14,4,30,2,10,12,2,24,6,16,8,10,2,12,22,6,2,16,20,10,2,12,12,18,10,12,6,2,10,2,6,10,18,2,12,6,4,6,2,24,28,2,4,2,10,2,16,12,8,22,2,6,4,2,10,6,20,12,10,8,12,6,6,6,4,18,2,4,12,18,2,12,6,4,2,16,12,12,14,4,8,18,4,12,14,6,6,4,8,6,4,20,12,10,14,4,2,16,2,12,30,4,6,24,20,24,10,8,12,10,12,6,12,12,6,8,16,14,6,4,6,36,20,10,30,12,2,4,2,28,12,14,6,22,8,4,18,6,14,18,4,6,2,6,34,18,2,16,6,18,2,24,4,2,6,12,6,12,10,8,6,16,12,8,10,14,40,6,2,6,4,12,14,4,2,4,2,4,8,6,10,6,6,2,6,6,6,12,6,24,10,2,10,6,12,6,6,14,6,6,52,20,6,10,2,10,8,10,12,12,2,6,4,14,16,8,12,6,22,2,10,8,6,22,2,22,6,8,10,12,12,2,10,6,12,2,4,14,10,2,6,18,4,12,8,18,12,6,6,4,6,6,14,4,2,12,12,4,6,18,18,12,2,16,12,8,18,10,26,4,6,8,6,6,4,2,10,20,4,6,8,4,20,10,2,34,2,4,24,2,12,12,10,6,2,12,30,6,12,16,12,2,22,18,12,14,10,2,12,12,4,2,4,6,12,2,16,18,2,40,8,16,6,8,10,2,4,18,8,10,8,12,4,18,2,18,10,2,4,2,4,8,28,2,6,22,12,6,14,18,4,6,8,6,6,10,8,4,2,18,10,6,20,22,8,6,30,4,2,4,18,6,30,2,4,8,6,4,6,12,14,34,14,6,4,2,6,4,14,4,2,6,28,2,4,6,8,10,2,10,2,10,2,4,30,2,12,12,10,18,12,14,10,2,12,6,10,6,14,12,4,14,4,18,2,10,8,4,8,10,12,18,18,8,6,18,16,14,6,6,10,14,4,6,2,12,12,4,6,6,12,2,16,2,12,6,4,14,6,4,2,12,18,4,36,18,12,12,2,4,2,4,8,12,4,36,6,18,2,12,10,6,12,24,8,6,6,16,12,2,18,10,20,10,2,6,18,4,2,40,6,2,16,2,4,8,18,10,12,6,2,10,8,4,6,12,2,10,18,8,6,4,20,4,6,36,6,2,10,6,24,6,14,16,6,18,2,10,20,10,8,6,4,6,2,10,2,12,4,2,4,8,10,6,12,18,14,12,16,8,6,16,8,4,2,6,18,24,18,10,12,2,4,14,10,6,6,6,18,12,2,28,18,14,16,12,14,24,12,22,6,2,10,8,4,2,4,14,12,6,4,6,14,4,2,4,30,6,2,6,10,2,30,22,2,4,6,8,6,6,16,12,12,6,8,4,2,24,12,4,6,8,6,6,10,2,6,12,28,14,6,4,12,8,6,12,4,6,14,6,12,10,6,6,8,6,6,4,2,4,8,12,4,14,18,10,2,16,6,20,6,10,8,4,30,36,12,8,22,12,2,6,12,16,6,6,2,18,4,26,4,8,18,10,8,10,6,14,4,20,22,18,12,8,28,12,6,6,8,6,12,24,16,14,4,14,12,6,10,12,20,6,4,8,18,12,18,10,2,4,20,10,14,4,6,2,10,24,18,2,4,20,16,14,10,14,6,4,6,20,6,10,6,2,12,6,30,10,8,6,4,6,8,40,2,4,2,12,18,4,6,8,10,6,18,18,2,12,16,8,6,4,6,6,2,52,14,4,20,16,2,4,6,12,2,6,12,12,6,4,14,10,6,6,14,10,14,16,8,6,12,4,8,22,6,2,18,22,6,2,18,6,16,14,10,6,12,2,6,4,8,18,12,16,2,4,14,4,8,12,12,30,16,8,4,2,6,22,12,8,10,6,6,6,14,6,18,10,12,2,10,2,4,26,4,12,8,4,18,8,10,14,16,6,6,8,10,6,8,6,12,10,20,10,8,4,12,26,18,4,12,18,6,30,6,8,6,22,12,2,4,6,6,2,10,2,4,6,6,2,6,22,18,6,18,12,8,12,6,10,12,2,16,2,10,2,10,18,6,20,4,2,6,22,6,6,18,6,14,12,16,2,6,6,4,14,12,4,2,18,16,36,12,6,14,28,2,12,6,12,6,4,2,16,30,8,24,6,30,10,2,18,4,6,12,8,22,2,6,22,18,2,10,2,10,30,2,28,6,14,16,6,20,16,2,6,4,32,4,2,4,6,2,12,4,6,6,12,2,6,4,6,8,6,4,20,4,32,10,8,16,2,22,2,4,6,8,6,16,14,4,18,8,4,20,6,12,12,6,10,2,10,2,12,28,12,18,2,18,10,8,10,48,2,4,6,8,10,2,10,30,2,36,6,10,6,2,18,4,6,8,16,14,16,6,14,4,20,4,6,2,10,12,2,6,12,6,6,4,12,2,6,4,12,6,8,4,2,6,18,10,6,8,12,6,22,2,6,12,18,4,14,6,4,20,6,16,8,4,8,22,8,12,6,6,16,12,18,30,8,4,2,4,6,26,4,14,24,22,6,2,6,10,6,14,6,6,12,10,6,2,12,10,12,8,18,18,10,6,8,16,6,6,8,16,20,4,2,10,2,10,12,6,8,6,10,20,10,18,26,4,6,30,2,4,8,6,12,12,18,4,8,22,6,2,12,34,6,18,12,6,2,28,14,16,14,4,14,12,4,6,6,2,36,4,6,20,12,24,6,22,2,16,18,12,12,18,2,6,6,6,4,6,14,4,2,22,8,12,6,10,6,8,12,18,12,6,10,2,22,14,6,6,4,18,6,20,22,2,12,24,4,18,18,2,22,2,4,12,8,12,10,14,4,2,18,16,38,6,6,6,12,10,6,12,8,6,4,6,14,30,6,10,8,22,6,8,12,10,2,10,2,6,10,2,10,12,18,20,6,4,8,22,6,6,30,6,14,6,12,12,6,10,2,10,30,2,16,8,4,2,6,18,4,2,6,4,26,4,8,6,10,2,4,6,8,4,6,30,12,2,6,6,4,20,22,8,4,2,4,72,8,4,8,22,2,4,14,10,2,4,20,6,10,18,6,20,16,6,8,6,4,20,12,22,2,4,2,12,10,18,2,22,6,18,30,2,10,14,10,8,16,50,6,10,8,10,12,6,18,2,22,6,2,4,6,8,6,6,10,18,2,22,2,16,14,10,6,2,12,10,20,4,14,6,4,36,2,4,6,12,2,4,14,12,6,4,6,2,6,4,20,10,2,10,6,12,2,24,12,12,6,6,4,24,2,4,24,2,6,4,6,8,16,6,2,10,12,14,6,34,6,14,6,4,2,30,22,8,4,6,8,4,2,28,2,6,4,26,18,22,2,6,16,6,2,16,12,2,12,4,6,6,14,10,6,8,12,4,18,2,10,8,16,6,6,30,2,10,18,2,10,8,4,8,12,24,40,2,12,10,6,12,2,12,4,2,4,6,18,14,12,6,4,14,30,4,8,10,8,6,10,18,8,4,14,16,6,8,4,6,2,10,2,12,4,2,4,6,8,4,6,32,24,10,8,18,10,2,6,10,2,4,18,6,12,2,16,2,22,6,6,8,18,4,18,12,8,6,4,20,6,30,22,12,2,6,18,4,62,4,2,12,6,10,2,12,12,28,2,4,14,22,6,2,6,6,10,14,4,2,10,6,8,10,14,10,6,2,12,22,18,8,10,18,12,2,12,4,12,2,10,2,6,18,6,6,34,6,2,12,4,6,18,18,2,16,6,6,8,6,10,18,8,10,8,10,2,4,18,26,12,22,2,4,2,22,6,6,14,16,6,20,10,12,2,18,42,4,24,2,6,10,12,2,6,10,8,4,6,12,12,8,4,6,12,30,20,6,24,6,10,12,2,10,20,6,6,4,12,14,10,18,12,8,6,12,4,14,10,2,12,30,16,2,12,6,4,2,4,6,26,4,18,2,4,6,14,54,6,52,2,16,6,6,12,26,4,2,6,22,6,2,12,12,6,10,18,2,12,12,10,18,12,6,8,6,10,6,8,4,2,4,20,24,6,6,10,14,10,2,22,6,14,10,26,4,18,8,12,12,10,12,6,8,16,6,8,6,6,22,2,10,20,10,6,44,18,6,10,2,4,6,14,4,26,4,2,12,10,8,4,8,12,4,12,8,22,8,6,10,18,6,6,8,6,12,4,8,18,10,12,6,12,2,6,4,2,16,12,12,14,10,14,6,10,12,2,12,6,4,6,2,12,4,26,6,18,6,10,6,2,18,10,8,4,26,10,20,6,16,20,12,10,8,10,2,16,6,20,10,20,4,30,2,4,8,16,2,18,4,2,6,10,18,12,14,18,6,16,20,6,4,8,6,4,6,12,8,10,2,12,6,4,2,6,10,2,16,12,14,10,6,8,6,28,2,6,18,30,34,2,16,12,2,18,16,6,8,10,8,10,8,10,44,6,6,4,20,4,2,4,14,28,8,6,16,14,30,6,30,4,14,10,6,6,8,4,18,12,6,2,22,12,8,6,12,4,14,4,6,2,4,18,20,6,16,38,16,2,4,6,2,40,42,14,4,6,2,24,10,6,2,18,10,12,2,16,2,6,16,6,8,4,2,10,6,8,10,2,18,16,8,12,18,12,6,12,10,6,6,18,12,14,4,2,10,20,6,12,6,16,26,4,18,2,4,32,10,8,6,4,6,6,14,6,18,4,2,18,10,8,10,8,10,2,4,6,2,10,42,8,12,4,6,18,2,16,8,4,2,10,14,12,10,20,4,8,10,38,4,6,2,10,20,10,12,6,12,26,12,4,8,28,8,4,8,24,6,10,8,6,16,12,8,10,12,8,22,6,2,10,2,6,10,6,6,8,6,4,14,28,8,16,18,8,4,6,20,4,18,6,2,24,24,6,6,12,12,4,2,22,2,10,6,8,12,4,20,18,6,4,12,24,6,6,54,8,6,4,26,36,4,2,4,26,12,12,4,6,6,8,12,10,2,12,16,18,6,8,6,12,18,10,2,54,4,2,10,30,12,8,4,8,16,14,12,6,4,6,12,6,2,4,14,12,4,14,6,24,6,6,10,12,12,20,18,6,6,16,8,4,6,20,4,32,4,14,10,2,6,12,16,2,4,6,12,2,10,8,6,4,2,10,14,6,6,12,18,34,8,10,6,24,6,2,10,12,2,30,10,14,12,12,16,6,6,2,18,4,6,30,14,4,6,6,2,6,4,6,14,6,4,8,10,12,6,32,10,8,22,2,10,6,24,8,4,30,6,2,12,16,8,6,4,6,8,16,14,6,6,4,2,10,12,2,16,14,4,2,4,20,18,10,2,10,6,12,30,8,18,12,10,2,6,6,4,12,12,2,4,12,18,24,2,10,6,8,16,8,6,12,10,14,6,12,6,6,4,2,24,4,6,8,6,4,2,4,6,14,4,8,10,24,24,12,2,6,12,22,30,2,6,18,10,6,6,8,4,2,6,10,8,10,6,8,16,6,14,6,4,24,8,10,2,12,6,4,36,2,22,6,8,6,10,8,6,12,10,14,10,6,18,12,2,12,4,26,10,14,16,18,8,18,12,12,6,16,14,24,10,12,8,22,6,2,10,60,6,2,4,8,16,14,10,6,24,6,12,18,24,2,30,4,2,12,6,10,2,4,14,6,16,2,10,8,22,20,6,4,32,6,18,4,2,4,2,4,8,52,14,22,2,22,20,10,8,10,2,6,4,14,4,6,20,4,6,2,12,12,6,12,16,2,12,10,8,4,6,2,28,12,8,10,12,2,4,14,28,8,6,4,2,4,6,2,12,58,6,14,10,2,6,28,32,4,30,8,6,4,6,12,12,2,4,6,6,14,16,8,30,4,2,10,8,6,4,6,26,4,12,2,10,18,12,12,18,2,4,12,8,12,10,20,4,8,16,12,8,6,16,8,10,12,14,6,4,8,12,4,20,6,40,8,16,6,36,2,6,4,6,2,22,18,2,10,6,36,14,12,4,18,8,4,14,10,2,10,8,4,2,18,16,12,14,10,14,6,6,42,10,6,6,20,10,8,12,4,12,18,2,10,14,18,10,18,8,6,4,14,6,10,30,14,6,6,4,12,38,4,2,4,6,8,12,10,6,18,6,50,6,4,6,12,8,10,32,6,22,2,10,12,18,2,6,4,30,8,6,6,18,10,2,4,12,20,10,8,24,10,2,6,22,6,2,18,10,12,2,30,18,12,28,2,6,4,6,14,6,12,10,8,4,12,26,10,8,6,16,2,10,18,14,6,4,6,14,16,2,6,4,12,20,4,20,4,6,12,2,36,4,6,2,10,2,22,8,6,10,12,12,18,14,24,36,4,20,24,10,6,2,28,6,18,8,4,6,8,6,4,2,12,28,18,14,16,14,18,10,8,6,4,6,6,8,22,12,2,10,18,6,2,18,10,2,12,10,18,32,6,4,6,6,8,6,6,10,20,6,12,10,8,10,14,6,10,14,4,2,22,18,2,10,2,4,20,4,2,34,2,12,6,10,2,10,18,6,14,12,12,22,8,6,16,6,8,4,12,6,8,4,36,6,6,20,24,6,12,18,10,2,10,26,6,16,8,6,4,24,18,8,12,12,10,18,12,2,24,4,12,18,12,14,10,2,4,24,12,14,10,6,2,6,4,6,26,4,6,6,2,22,8,18,4,18,8,4,24,2,12,12,4,2,52,2,18,6,4,6,12,2,6,12,10,8,4,2,24,10,2,10,2,12,6,18,40,6,20,16,2,12,6,10,12,2,4,6,14,12,12,22,6,8,4,2,16,18,12,2,6,16,6,2,6,4,12,30,8,16,2,18,10,24,2,6,24,4,2,22,2,16,2,6,12,4,18,8,4,14,4,18,24,6,2,6,10,2,10,38,6,10,14,6,6,24,4,2,12,16,14,16,12,2,6,10,26,4,2,12,6,4,12,8,12,10,18,6,14,28,2,6,10,2,4,14,34,2,6,22,2,10,14,4,2,16,8,10,6,8,10,8,4,6,2,16,6,6,18,30,14,6,4,30,2,10,14,4,20,10,8,4,8,18,4,14,6,4,24,6,6,18,18,2,36,6,10,14,12,4,6,2,30,6,4,2,6,28,20,4,20,12,24,16,18,12,14,6,4,12,32,12,6,10,8,10,6,18,2,16,14,6,22,6,12,2,18,4,8,30,12,4,12,2,10,38,22,2,4,14,6,12,24,4,2,4,14,12,10,2,16,6,20,4,20,22,12,2,4,2,12,22,24,6,6,2,6,4,6,2,10,12,12,6,2,6,16,8,6,4,18,12,12,14,4,12,6,8,6,18,6,10,12,14,6,4,8,22,6,2,28,18,2,18,10,6,14,10,2,10,14,6,10,2,22,6,8,6,16,12,8,22,2,4,14,18,12,6,24,6,10,2,12,22,18,6,20,6,10,14,4,2,6,12,22,14,12,4,6,8,22,2,10,12,8,40,2,6,10,8,4,42,20,4,32,12,10,6,12,12,2,10,8,6,4,8,4,26,18,4,8,28,6,18,6,12,2,10,6,6,14,10,12,14,24,6,4,20,22,2,18,4,6,12,2,16,18,14,6,6,4,6,8,18,4,14,30,4,18,8,10,2,4,8,12,4,12,18,2,12,10,2,16,8,4,30,2,6,28,2,10,2,18,10,14,4,26,6,18,4,20,6,4,8,18,4,12,26,24,4,20,22,2,18,22,2,4,12,2,6,6,6,4,6,14,4,24,12,6,18,2,12,28,14,4,6,8,22,6,12,18,8,4,20,6,4,6,2,18,6,4,12,12,8,28,6,8,10,2,24,12,10,24,8,10,20,12,6,12,12,4,14,12,24,34,18,8,10,6,18,8,4,8,16,14,6,4,6,24,2,6,4,6,2,16,6,6,20,24,4,2,4,14,4,18,2,6,12,4,14,4,2,18,16,6,6,2,16,20,6,6,30,4,8,6,24,16,6,6,8,12,30,4,18,18,8,4,26,10,2,22,8,10,14,6,4,18,8,12,28,2,6,4,12,6,24,6,8,10,20,16,8,30,6,6,4,2,10,14,6,10,32,22,18,2,4,2,4,8,22,8,18,12,28,2,16,12,18,14,10,18,12,6,32,10,14,6,10,2,10,2,6,22,2,4,6,8,10,6,14,6,4,12,30,24,6,6,8,6,4,2,4,6,8,6,6,22,18,8,4,2,18,6,4,2,16,18,20,10,6,6,30,2,12,28,6,6,6,2,12,10,8,18,18,4,8,18,10,2,28,2,10,14,4,2,30,12,22,26,10,8,6,10,8,16,14,6,6,10,14,6,4,2,10,12,2,6,10,8,4,2,10,26,22,6,2,12,18,4,26,4,8,10,6,14,10,2,18,6,10,20,6,6,4,24,2,4,8,6,16,14,16,18,2,4,12,2,10,2,6,12,10,6,6,20,6,4,6,38,4,6,12,14,4,12,8,10,12,12,8,4,6,14,10,6,12,2,10,18,2,18,10,8,10,2,12,4,14,28,2,16,2,18,6,10,6,8,16,14,30,10,20,6,10,24,2,28,2,12,16,6,8,36,4,8,4,14,12,10,8,12,4,6,8,4,6,14,22,8,6,4,2,10,6,20,10,8,6,6,22,18,2,16,6,20,4,26,4,14,22,14,4,12,6,8,4,6,6,26,10,2,18,18,4,2,16,2,18,4,6,8,4,6,12,2,6,6,28,38,4,8,16,26,4,2,10,12,2,10,8,6,10,12,2,10,2,24,4,30,26,6,6,18,6,6,22,2,10,18,26,4,18,8,6,6,12,16,6,8,16,6,8,16,2,42,58,8,4,6,2,4,8,16,6,20,4,12,12,6,12,2,10,2,6,22,2,10,6,8,6,10,14,6,6,4,18,8,10,8,16,14,10,2,10,2,12,6,4,20,10,8,52,8,10,6,2,10,8,10,6,6,8,10,2,22,2,4,6,14,4,2,24,12,4,26,18,4,6,14,30,6,4,6,2,22,8,4,6,2,22,6,8,16,6,14,4,6,18,8,12,6,12,24,30,16,8,34,8,22,6,14,10,18,14,4,12,8,4,36,6,6,2,10,2,4,20,6,6,10,12,6,2,40,8,6,28,6,2,12,18,4,24,14,6,6,10,20,10,14,16,14,16,6,8,36,4,12,12,6,12,50,12,6,4,6,6,8,6,10,2,10,2,18,10,14,16,8,6,4,20,4,2,10,6,14,18,10,38,10,18,2,10,2,12,4,2,4,14,6,10,8,40,6,20,4,12,8,6,34,8,22,8,12,10,2,16,42,12,8,22,8,22,8,6,34,2,6,4,14,6,16,2,22,6,8,24,22,6,2,12,4,6,14,4,8,24,4,6,6,2,22,20,6,4,14,4,6,6,8,6,10,6,8,6,16,14,6,6,22,6,24,32,6,18,6,18,10,8,30,18,6,16,12,6,12,2,6,4,12,8,6,22,8,6,4,14,10,18,20,10,2,6,4,2,28,18,2,10,6,6,6,14,40,24,2,4,8,12,4,20,4,32,18,16,6,36,8,6,4,6,14,4,6,26,6,10,14,18,10,6,6,14,10,6,6,14,6,24,4,14,22,8,12,10,8,12,18,10,18,8,24,10,8,4,24,6,18,6,2,10,30,2,10,2,4,2,40,2,28,8,6,6,18,6,10,14,4,18,30,18,2,12,30,6,30,4,18,12,2,4,14,6,10,6,8,6,10,12,2,6,12,10,2,18,4,20,4,6,14,6,6,22,6,6,8,18,18,10,2,10,2,6,4,6,12,18,2,10,8,4,18,2,6,6,6,10,8,10,6,18,12,8,12,6,4,6,14,16,2,12,4,6,38,6,6,16,20,28,20,10,6,6,14,4,26,4,14,10,18,14,28,2,4,14,16,2,28,6,8,6,34,8,4,18,2,16,8,6,40,8,18,4,30,6,12,2,30,6,10,14,40,14,10,2,12,10,8,4,8,6,6,28,2,4,12,14,16,8,30,16,18,2,10,18,6,32,4,18,6,2,12,10,18,2,6,10,14,18,28,6,8,16,2,4,20,10,8,18,10,2,10,8,4,6,12,6,20,4,2,6,4,20,10,26,18,10,2,18,6,16,14,4,26,4,14,10,12,14,6,6,4,14,10,2,30,18,22,2,16,2,4,8,6,6,16,2,6,12,10,8,12,4,14,4,6,20,10,12,2,6,6,4,2,10,2,30,16,12,20,18,4,6,2,4,8,16,14,18,22,6,2,22,6,6,18,2,10,36,8,4,6,20,4,12,6,14,4,2,28,24,8,4,6,12,30,18,32,22,8,36,6,4,12,2,12,4,6,20,10,18,18,8,6,4,24,8,10,14,6,4,8,12,16,2,16,6,8,16,12,14,10,30,14,4,12,8,12,6,10,2,12,28,6,12,12,20,10,2,10,14,6,6,30,4,8,12,4,2,10,14,4,26,18,12,10,6,8,4,12,6,24,18,8,10,2,12,4,12,12,6,2,22,2,4,2,12,16,14,10,2,16,18,32,4,6,20,22,8,10,2,10,6,2,4,14,6,24,4,8,4,6,12,12,8,6,10,12,8,10,2,10,12,6,12,12,20,28,20,10,14,10,8,10,6,2,4,14,6,6,12,6,12,10,14,10,14,16,8,10,26,4,2,6,4,14,4,6,12,8,6,30,18,12,6,12,16,12,12,2,28,6,14,10,36,2,4,6,8,12,22,18,2,30,18,22,20,18,10,38,6,4,2,24,4,6,6,2,10,6,14,10,8,4,24,14,16,14,22,6,20,10,14,4,12,12,2,16,8,6,6,18,4,6,14,22,6,2,42,16,2,10,6,2,4,6,8,10,20,16,30,8,10,8,10,2,30,6,6,36,10,8,16,6,2,12,28,2,4,6,18,12,6,8,10,2,4,50,4,20,4,30,8,4,6,12,2,24,4,8,18,6,4,6,8,10,2,4,2,40,18,36,30,30,8,16,14,6,12,28,2,22,2,4,12,30,12,6,2,4,14,10,2,18,22,12,18,2,10,18,32,6,4,2,6,10,20,12,10,6,12,20,12,6,4,2,16,2,16,6,14,4,2,16,2,6,16,6,8,4,8,22,18,8,12,4,8,6,24,22,6,2,12,30,6,10,12,6,2,22,6,2,12,6,22,8,12,22,2,10,6,18,12,2,6,12,18,6,4,20,22,8,12,24,16,14,10,30,18,2,6,4,14,10,2,12,10,12,6,2,16,12,2,6,12,10,2,10,6,2,12,12,16,20,10,12,8,30,10,14,4,6,8,6,4,20,18,24,4,12,8,4,2,24,6,24,10,2,4,6,2,6,6,6,4,24,2,10,12,2,6,10,8,6,10,18,2,6,4,20,24,10,12,2,12,6,24,4,36,14,16,8,22,6,8,4,2,6,22,20,16,12,18,2,12,16,6,6,12,6,12,2,6,12,10,8,16,8,6,16,8,12,4,6,6,20,12,12,4,6,20,4,12,2,10,2,6,30,22,6,2,4,38,10,2,4,2,22,2,16,2,6,10,20,6,24,4,12,14,12,4,38,10,30,6,2,12,12,4,6,30,14,4,8,18,36,4,6,20,4,2,12,10,2,6,10,12,6,12,8,6,6,24,4,30,20,6,36,10,2,12,6,4,8,6,4,12,8,6,12,4,6,14,4,20,12,4,6,18,2,4,18,2,16,12,30,6,6,8,40,8,48,6,16,18,14,12,6,18,4,20,10,2,6,10,8,30,4,12,20,6,12,6,6,34,6,6,18,6,8,10,12,6,8,10,2,4,24,6,8,22,6,2,12,6,10,12,6,24,6,14,12,36,4,24,2,10,8,10,6,14,10,32,4,8,10,12,26,18,4,6,20,4,20,6,16,6,2,30,12,6,10,2,6,10,12,8,4,2,6,10,12,26,22,8,6,4,14,6,6,30,4,6,14,4,2,28,2,6,22,8,4,18,18,18,2,12,6,4,20,10,6,6,14,10,12,2,12,30,34,12,8,6,4,2,10,2,16,12,2,10,8,18,24,6,4,12,14,4,8,4,14,4,6,6,20,6,4,8,18,52,2,4,12,8,4,38,4,26,24,16,12,6,2,12,12,16,2,6,6,4,12,14,16,8,12,18,16,6,8,10,6,14,10,12,2,10,2,4,24,6,42,24,8,10,6,6,6,2,12,4,14,6,6,28,6,2,10,12,12,6,20,4,6,14,4,2,12,10,12,24,6,8,6,6,4,24,12,20,16,14,30,18,6,4,26,12,4,6,2,6,4,2,28,8,40,2,10,8,4,20,6,18,10,2,4,44,6,18,12,6,4,6,2,22,6,14,30,10,24,2,10,8,16,18,2,18,22,8,10,6,6,14,4,8,18,4,2,18,18,18,6,4,24,18,2,16,6,6,18,20,16,20,4,14,6,4,20,18,10,2,6,10,24,2,10,24,6,6,24,6,12,2,28,12,14,6,6,12,6,22,12,12,8,36,4,12,14,4,20,10,12,24,2,4,6,12,2,4,2,10,12,26,6,16,8,4,8,10,8,6,34,2,12,16,24,6,2,10,2,18,4,8,6,16,6,2,6,6,6,4,14,4,20,6,4,20,6,12,22,6,2,10,12,2,6,4,8,12,4,14,12,10,14,4,12,26,10,14,4,26,6,30,4,18,18,8,6,16,8,10,14,10,8,10,20,22,20,16,2,18,6,4,6,6,12,2,10,26,4,8,18,18,6,18,6,4,6,24,6,20,34,26,10,2,28,12,8,10,12,2,6,22,2,12,16,2,6,6,10,14,16,20,6,4,38,6,10,6,8,16,42,2,6,4,6,6,6,14,16,14,4,20,10,2,4,8,18,10,12,36,2,10,42,8,4,20,24,16,8,22,6,8,4,2,6,22,6,6,8,28,2,10,18,14,6,4,18,8,10,14,4,12,8,10,12,14,4,2,12,12,4,6,18,30,12,38,6,12,10,2,18,10,12,8,4,8,6,4,2,24,12,18,4,2,4,2,58,12,8,24,10,2,4,6,6,12,2,4,14,6,6,16,12,2,4,32,4,24,6,6,8,10,2,22,18,12,20,6,30,4,30,6,2,4,14,6,4,14,16,2,12,10,2,6,12,12,10,6,8,22,8,12,12,6,16,6,18,20,22,18,2,22,2,16,2,22,14,10,20,10,32,4,8,10,6,2,22,6,12,2,6,4,2,4,14,12,24,10,2,12,16,2,4,6,14,6,10,12,2,16,14,34,12,2,6,6,6,4,20,10,26,12,12,4,2,4,8,10,2,4,2,22,6,6,14,4,18,12,26,6,10,8,16,2,4,20,10,6,42,2,10,6,8,24,12,6,4,6,12,2,28,8,12,18,18,6,46,8,10,6,14,4,2,6,4,6,42,8,10,8,10,2,18,4,6,12,12,2,4,20,10,12,12,8,4,26,18,22,8,6,16,14,16,2,18,10,2,6,6,10,14,4,2,30,4,2,4,8,10,6,2,12,16,6,56,10,2,12,10,8,12,6,4,14,10,2,4,8,6,4,20,6,12,22,6,32,10,2,10,12,14,6,28,36,6,6,2,12,4,6,6,8,22,2,18,10,2,6,4,20,10,8,4,6,14,18,6,42,22,2,4,2,28,2,4,18,6,6,6,12,2,24,10,36,6,2,12,10,26,24,18,16,6,6,14,24,12,4,8,6,12,4,8,16,20,40,26,4,12,2,6,4,2,10,14,10,2,4,26,12,28,2,16,26,6,10,2,6,10,6,8,6,6,6,10,12,6,20,40,20,4,2,16,12,6,12,8,4,18,2,12,10,26,12,16,2,18,24,12,4,14,22,20,10,14,12,4,18,12,8,10,12,6,30,14,4,24,6,30,6,6,2,6,22,32,6,4,6,6,20,16,2,10,8,12,10,2,6,10,8,16,36,8,6,4,2,28,2,28,12,2,10,6,14,10,6,6,6,8,6,4,14,18,4,6,12,2,10,18,8,30,40,2,18,4,6,14,18,6,4,12,6,12,6,14,10,26,6,16,2,16,30,2,10,2,42,6,28,14,6,10,2,12,18,12,6,10,12,12,20,6,4,2,10,6,12,12,14,12,34,6,2,12,10,6,8,6,4,12,38,6,10,18,2,28,2,6,12,30,16,2,10,8,4,2,16,18,26,4,6,8,18,22,6,20,4,6,12,2,6,12,4,18,6,2,22,12,8,6,16,18,30,12,24,2,10,2,6,6,4,6,36,14,6,22,2,58,8,12,6,10,2,40,8,6,28,2,4,14,6,6,18,10,8,4,14,4,8,30,4,6,8,6,6,18,4,2,4,14,12,18,10,2,4,12,2,10,8,10,14,10,18,12,8,6,10,14,10,8,22,2,6,22,12,6,8,12,28,2,48,12,4,18,8,10,14,10,14,4,12,30,24,6,8,6,4,8,54,4,2,10,12,8,10,12,12,18,2,24,4,8,22,12,20,4,12,2,12,16,2,28,2,6,24,10,2,28,2,4,20,4,12,6,14,4,6,14,22,24,20,4,14,6,6,10,30,8,10,18,2,6,6,16,2,6,6,4,2,24,4,2,24,10,6,2,10,2,6,22,8,4,8,6,4,18,2,18,4,8,16,26,4,6,8,22,20,16,8,4,6,24,6,14,12,16,2,12,4,14,10,2,4,12,18,32,10,14,24,12,40,8,34,12,14,4,18,2,28,12,20,6,10,2,40,18,14,12,4,36,6,2,22,6,14,10,24,42,2,16,2,34,8,6,4,2,4,14,40,8,12,6,24,18,4,6,2,6,4,2,4,2,24,10,8,6,6,10,14,6,16,18,14,18,24,4,6,6,8,4,20,10,6,12,2,12,4,14,6,6,6,4,14,16,36,14,6,4,14,4,6,24,8,4,20,10,14,12,34,8,10,6,6,6,14,4,14,12,6,10,18,14,10,12,6,2,6,6,28,2,4,24,6,2,4,8,16,6,20,4,2,10,2,10,8,64,6,8,12,4,14,12,10,2,12,6,10,18,24,6,2,10,8,6,16,20,4,14,6,6,12,6,4,6,2,4,8,22,6,8,4,2,16,18,14,6,22,14,10,14,4,6,2,4,14,10,12,8,16,8,10,8,24,40,6,12,2,6,18,4,2,4,30,2,30,4,8,18,12,12,4,2,4,14,36,16,18,2,12,10,6,12,18,2,18,6,6,22,18,38,6,10,18,2,10,8,6,16,24,14,6,4,6,14,16,24,6,12,8,12,10,14,46,2,16,2,22,6,2,10,2,10,2,6,4,20,10,6,30,8,6,6,4,30,8,6,6,6,22,36,2,4,8,6,6,4,14,12,10,20,4,2,4,30,6,14,16,12,30,2,4,6,8,30,10,8,34,18,12,8,22,20,4,14,10,20,6,4,2,10,14,4,26,6,36,12,18,4,8,6,4,6,2,28,6,6,24,8,10,26,6,24,4,8,24,10,20,4,2,10,14,16,2,6,6,4,6,8,18,28,14,6,16,14,6,4,6,6,8,4,2,4,12,2,12,6,12,28,2,6,12,10,14,4,44,6,10,2,12,12,30,4,12,2,6,10,12,2,10,2,10,6,8,10,6,14,16,8,6,12,10,2,10,8,12,10,18,8,4,2,4,26,6,22,6,14,10,6,2,28,6,8,46,6,6,18,6,6,8,6,10,18,2,6,12,18,10,8,12,30,10,2,10,2,4,6,18,2,4,20,12,4,6,8,34,6,6,24,12,8,36,16,2,6,4,2,4,6,20,6,24,4,2,4,18,20,6,22,8,46,18,2,16,20,22,2,24,22,2,16,24,20,16,2,4,8,10,2,10,14,4,8,18,4,8,4,14,10,2,24,16,8,6,16,20,10,2,6,4,30,2,16,32,6,12,10,24,8,12,18,16,2,12,6,4,12,6,2,28,18,2,22,6,6,6,2,6,16,14,6,30,16,2,10,2,4,12,2,12,10,14,6,10,8,28,2,36,6,16,14,4,20,24,6,4,8,4,18,8,4,14,4,6,2,24,16,14,4,26,16,2,10,32,6,4,6,12,6,36,8,12,4,2,4,8,6,4,20,12,10,24,12,2,12,10,6,12,2,6,18,4,6,6,6,8,24,6,10,12,30,14,10,8,12,6,10,12,2,18,6,4,8,4,24,20,4,8,10,12,8,12,16,6,14,4,8,4,18,50,6,6,4,6,8,6,10,26,10,6,2,10,2,10,6,38,12,4,8,10,20,6,6,6,18,10,2,12,16,2,12,12,4,26,10,6,20,18,40,12,8,10,12,2,18,12,10,2,10,26,4,6,12,8,4,30,6,2,6,16,24,24,18,12,12,8,6,4,8,10,8,6,4,20,10,26,4,24,6,2,12,42,18,6,4,26,6,28,6,2,10,8,6,6,10,8,10,2,22,2,4,20,4,6,36,14,4,20,22,6,14,6,10,8,4,2,4,14,18,34,8,22,14,10,24,6,2,10,2,6,10,26,18,10,18,24,18,2,24,40,2,4,6,2,6,10,26,6,12,12,6,4,36,2,10,12,24,2,4,8,10,6,2,4,24,2,4,36,2,22,14,24,18,42,6,10,2,24,16,12,2,4,2,10,2,10,8,4,36,8,4,12,18,6,6,14,22,2,6,24,6,10,24,20,22,6,14,36,28,6,8,6,24,6,12,28,2,18,4,2,4,20,22,8,10,2,18,4,8,10,14,10,6,8,6,6,12,16,12,14,10,18,2,10,24,24,6,12,2,22,6,20,22,2,4,12,2,6,36,6,22,6,2,28,12,18,2,4,14,6,4,2,10,2,16,2,10,8,6,10,18,12,6,14,4,6,18,12,26,4,6,14,6,10,12,2,4,2,10,24,8,10,32,10,8,10,6,2,18,12,28,30,2,18,4,6,14,6,4,8,22,8,30,18,10,26,4,2,22,8,4,8,6,4,26,4,12,20,18,6,12,10,18,2,4,6,2,12,28,6,20,6,16,8,6,6,4,6,20,12,6,4,20,6,16,6,32,10,18,2] :: [Int]
