{-# LANGUAGE ViewPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.Maybe

mergeHelper d x m@(IntMap.lookupLT x -> Nothing) = m
mergeHelper d x m@(IntMap.lookupLT x -> Just (k, v))
  | x >= v = m
  | x - k <= d && v - x <= d = IntMap.delete k m
  | x - k <= d = IntMap.insert x v . IntMap.delete k $ m
  | v - x <= d = IntMap.insert k x . IntMap.delete k $ m
  | otherwise  =  IntMap.insert k x . IntMap.insert x v . IntMap.delete k $ m

claw :: Int -> Int -> Int -> [[Int]] -> Int
claw n w d = (\(r, b) -> if b then r else -1) . fst . foldl go ((0, False), IntMap.singleton 0 w)
  where go done@( (_, True), _ ) _ = done
        go notyet@( (t, False), v) s = ( (succ t, IntMap.null v'), v')
          where v'= foldl (flip (mergeHelper d)) v s

readint = fst . fromJust . C.readInt
processinputs 0 _ = return ()
processinputs k (h:s) = print (claw intervals width cap stones) >> processinputs (pred k) s2
  where (intervals:width:cap:_) = map readint (C.words h)
        (s1, s2) = splitAt intervals s
        stones = map readstones s1
          where readstones = tail . map readint . C.words

processall s = processinputs (readint l1) $ rest
  where (l1:rest) = C.lines s

main = C.getContents >>= processall
