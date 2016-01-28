import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.Maybe

mergeHelper d x m = case IntMap.lookupLT x m of
  Nothing -> m
  Just (k, v) -> if x >= v then m else
                   if x - k <= d && v - x <= d then IntMap.delete k m else
                     if x - k <= d then IntMap.insert x v (IntMap.delete k m) else
                       if v - x <= d then IntMap.insert k x (IntMap.delete k m) else
                         IntMap.insert k x (IntMap.insert x v (IntMap.delete k m))
                    
claw :: Int -> Int -> Int -> [[Int]] -> Int
claw n w d = (\(r, b) -> if b then r else -1) . fst . foldl go ((0, False), IntMap.singleton 0 w)
  where go done@( (_, True), _ ) _ = done
        go notyet@( (t, False), v) s = ( (succ t, IntMap.null v'), v')
          where v'= foldl (flip (mergeHelper d)) v s

readint = fst . fromJust . C.readInt

processinputs 0 _ = []
processinputs k (h:s) = (claw intervals width cap stones) : processinputs (pred k) s2
  where (intervals:width:cap:_) = map readint (C.words h)
        (s1, s2) = splitAt intervals s
        stones = map readstones s1
          where readstones = tail . map readint . C.words

processall s = putStr . foldr p1 "" . processinputs (readint l1) $ rest
  where (l1:rest) = C.lines s
        p1 x = shows x . showString "\n"

main = C.getContents >>= processall
