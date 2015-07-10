{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as Map
import Data.Array.Unboxed

facts = listArray (1, 20) [
    [ (1, 1) ]
  , [ (2, 1) ]
  , [ (3, 1) ]
  , [ (2, 2) ]
  , [ (5, 1) ]
  , [ (2, 1), (3, 1) ]
  , [ (7, 1) ]
  , [ (2, 3) ]
  , [ (3, 2) ]
  , [ (2, 1), (5, 1)]
  , [ (11, 1) ]
  , [ (2, 2), (3, 1)]
  , [ (13, 1) ]
  , [ (2, 1), (7, 1)]
  , [ (3, 1), (5, 1)]
  , [ (2, 4) ]
  , [ (17, 1) ]
  , [ (2, 1), (3, 2)]
  , [ (19, 1) ]
  , [ (2, 2), (5, 1) ]
    ]:: Array Int [ (Int, Int) ]

--facts = amap Map.fromList facts0

readg = C.foldr go []
  where go c r
          | c == 'Y' = True : r
          | otherwise = False : r

yesdata = [
    (20, [10, 5, 4, 2])
  , (18, [9, 6, 3, 2])
  , (16, [8, 4, 2])
  , (15, [5, 3])
  , (14, [7, 2])
  , (12, [6, 4, 3, 2])
  , (10, [5, 2])
  , (9, [3])
  , (8, [4, 2])
  , (6, [3, 2])
  , (4, [2])
    ] :: [ (Int, [Int]) ]

nodata = [
    (20, [4, 5])
  , (18, [2, 9])
  , (15, [3, 5])
  , (14, [2, 7])
  , (12, [3, 4])
  , (10, [2, 5])
  , (6, [2, 3])
    ] :: [ (Int, [Int]) ]

validate u = if (u!1 == False) then False else go hi
  where (lo, hi) = bounds u
        go k
          | k < lo = True
          | u ! k == False = case lookup k nodata of
            Nothing -> go (pred k)
            Just xs -> if all (== True) (map (u!) xs) then False else go (pred k)
          | u ! k == True = case lookup k yesdata of
            Nothing -> go (pred k)
            Just xs -> if all (== True) (map (u!) xs) then go (pred k) else False

readgu s = listArray (1, length l) l :: UArray Int Bool
  where l = readg s

guess s
  | validate s == False = -1
  | otherwise = let f = Map.fromListWith max (concatMap (facts!) l)
                    go k v r = r*(k^v)
                    l = genl 2
                    !(_, !hi) = bounds s
                    genl k = if k > hi then [] else
                               let !v = s ! k
                               in if v then k: genl (succ k) else genl (succ k)
                in Map.foldrWithKey go 1 f


pr :: [Int] -> String
pr = foldr p1 ""
  where p1 x = shows x . showString "\n"

main = C.getContents >>= putStr . pr . map (guess . readgu) . init . C.lines
