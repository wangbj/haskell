{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.Maybe

spcu s = go 0 s
  where go k [] = True
        go k (x:xs)
          | x > k = False
          | otherwise = go (succ k) xs

readint = fst . fromJust . C.readInt

unline s = (l1, rest)
  where (!l1, !r1) = C.break (== '\n') s
        (_, !rest) = C.span (== '\n') r1

processinputs :: Int -> [C.ByteString] -> [Bool]
processinputs 0 _ = []
processinputs k (_:ints:rest) = spcu (map readint (C.words ints)) : processinputs (pred k) rest

processall s = processinputs (readint l1) (C.lines rest)
  where (l1, rest) = unline s

pr :: [Bool] -> IO ()
pr = putStr . foldr p1 ""
  where p1 True = showString "YES\n"
        p1 False = showString "NO\n"

main = C.getContents >>= pr . processall
