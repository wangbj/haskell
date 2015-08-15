{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.IntMap(IntMap)
import Data.Set(Set)
import Data.Int
import Data.Maybe

ger :: [Int64] -> Int64
ger = fst . foldl go (0, 0)
  where go (!r, !x) y = (abs r + abs x, x + y)

unline s = (l1, C.tail r1)
  where (!l1, r1) = C.break (== '\n') s

readint = fromIntegral . fst . fromJust . C.readInteger

process [] = []
process (_:ints:rest) = ger (map readint (C.words ints)) : process rest

pr = foldr p1 ""
  where p1 x = shows x . showString "\n"

main = C.getContents >>= putStr . pr . process . init . C.lines
