{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import Data.Maybe
import Data.List

readpair s = fromJust $ C.readInt s >>= \(x, s1) ->
  (C.readInt . C.tail) s1 >>= \(y, s2) ->
  return $! (x, y)

readint = fst . fromJust . C.readInt

busy = fst . foldl go (1, (-1, maxBound))
  where go (!r, !pp@(ppx, ppy)) p@(px, py)
          | ppx == px = (r, pp) -- already sorted
          | ppy <= px = (succ r, p)
          | py <= ppy = (r, p)
          | otherwise = (r, pp)

processinputs 0 _ = []
processinputs n (k:s) = busy ps : processinputs (pred n) rest
  where (s1, rest) = splitAt (readint k) s
        ps = IntMap.assocs . IntMap.fromListWith min . map readpair $ s1

processall txts = processinputs (readint nt) rest
  where (nt:rest) = C.lines txts

pr = foldr p1 ""
  where p1 x = shows x . showString "\n"

main = C.getContents >>= putStr . pr . processall
