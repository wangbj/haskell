{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.Char

toInt c = ord c - ord '0'

norm s1 s2 = (
    C.concat [C.replicate (l - l1) '0', s1]
  , C.concat [C.replicate (l - l2) '0', s2] )
  where !l1 = C.length s1
        !l2 = C.length s2
        !l = max l1 l2

carry s1 = fst . foldr go (0, 0) . C.zipWith (\x y -> (toInt x, toInt y)) s1
  where go (!x, !y) (!r, !c) 
          | x + y + c >= 10 = (succ r, 1)
          | otherwise = (r, 0)

readpair s = (s1, s2)
  where (s1:s2:_) = C.words s

prof = uncurry carry . uncurry norm . readpair

pr 0 = "No carry operation.\n"
pr 1 = "1 carry operation.\n"
pr k = show k ++ " carry operations.\n"

main = C.getContents >>= mapM_ (putStr . pr . prof) . init . C.lines
