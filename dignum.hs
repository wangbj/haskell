{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder
import Data.Bits
import Data.Maybe(fromJust)

data DL = DL {
    _d_1 :: {-# UNPACK #-} !Int
  , _d_2 :: {-# UNPACK #-} !Int
  , _d_3 :: {-# UNPACK #-} !Int  
  } deriving Show

d1 ' ' = 1
d1 '_' = 2
d1 '|' = 3

digits = [
    " _ " ++ "   " ++ " _ " ++ " _ " ++ "   " ++ " _ " ++ " _ " ++ " _ " ++ " _ " ++ " _ " 
  , "| |" ++ "  |" ++ " _|" ++ " _|" ++ "|_|" ++ "|_ " ++ "|_ " ++ "  |" ++ "|_|" ++ "|_|"
  , "|_|" ++ "  |" ++ "|_ " ++ " _|" ++ "  |" ++ " _|" ++ "|_|" ++ "  |" ++ "|_|" ++ "  |"
    ]

-- | map dl' . group 3 . digits $ [0, 1, 2] =>
--l1 = [25,21,25,25,21,25,25,25,25,25] :: [Int]
--l2 = [55,23,27,27,59,57,57,23,59,59] :: [Int]
--l3 = [59,23,57,27,23,27,59,23,59,23] :: [Int]
-- =>
-- | 0 = da (DL 25 55 59) ...
-- =>
--dict = listArray (0, 9) (map da (zipWith3 DL l1 l2 l3)) :: UArray Int Int

line = thd . C.foldr go (1, 0, [])
  where go c !(!k, !r, !t)
          | k == 3 = (1, 0, ( ((d1 c) `shiftL` (2*k - 2) ) .|. r) : t)
          | otherwise = (succ k, ((d1 c) `shiftL` (2*k - 2)) .|. r, t)
        go :: Char -> (Int, Int, [Int])  -> (Int, Int, [Int])
        thd (_, _, !x) = x

decode = flip lookup rdict
  where rdict = zip [105979,87511,104185,104155,89815,106075,106107,103895,106235,106199] [0..9] :: [(Int, Int)]
        -- zipWith3 da l1 l2 l3

da x y z =  (x `shiftL` 12) .|.
      (y `shiftL` 6) .|.
      z

dg x y z = fromJust . decode $ r
  where r = (x `shiftL` 12) .|.
            (y `shiftL` 6) .|.
            z

showl :: [Int] -> String
showl = foldr p1 ""
  where p1 x = shows x

dignum s = if C.null s then [] else (showl $ zipWith3 dg (line l1) (line l2) (line l3)) : dignum (C.tail r3)
  where (l1, r1) = C.break (== '\n') s
        (l2, r2) = C.break (== '\n') (C.tail r1)
        (l3, r3) = C.break (== '\n') (C.tail r2)

main = C.getContents >>= mapM_ putStrLn . dignum
