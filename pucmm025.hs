{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.Array.Unboxed
import Data.Maybe(fromJust)
import Data.Char

readint = fst . fromJust . C.readInt

dig x = ord x - ord '0'

div7 = (\x -> x`mod`7 == 0) . snd . C.foldr go (1, 0)
  where go c (!k, !r) = (k', r+(u!k) * (dig c))
          where !k' = if k == 6 then 1 else succ k
        !u = listArray (1, 6) [1, 3, 2, -1, -3, -2] :: UArray Int Int

div8 s = (\x -> x`mod`8 == 0) . readint . C.drop (len-3) $ s
  where !len = C.length s

div4 s = (\x -> x`mod`4 == 0) . readint . C.drop (len-2) $ s
  where !len = C.length s

div2 = even . dig . C.last

div9 = (\x -> x`mod`9 == 0) . digsum

digsum = C.foldl go 0
  where go r c = r + dig c

div6 s = div2 s && div3 s

div3 = (\x -> x`mod`3 == 0) . digsum

div5 = (\x -> x == 0 || x == 5) . dig . C.last

gencache s = listArray (1, 9) [d1, d2, d3, d4, d5, d6, d7, d8, d9] :: UArray Int Bool
  where ds = digsum s
        d1 = True
        d2 = div2 s
        d3 = ds `mod` 3 == 0
        d4 = div4 s
        d5 = div5 s
        d6 = d2 && d3
        d7 = div7 s
        d8 = div8 s
        d9 = ds `mod` 9 == 0

divdig s = C.foldl go 0 s
  where !u = gencache s
        go r c
          | v == True = succ r
          | v == False = r
          where v = u ! (dig c)

pr = foldr p1 ""
  where p1 x = shows x . showString "\n"

main = C.getContents >>= putStr. pr . map divdig . C.lines
