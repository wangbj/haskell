{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.Maybe

deleteFindMin (IntMap.minViewWithKey -> Just ((k, v), m))
  | v == 1 = (k, m)
  | otherwise = (k, IntMap.insert k (v-1) m)

iter !m = if IntMap.null m then 0 else 1 + iter (uncurry go (deleteFindMin m))
  where go !x !m = if IntMap.null m then IntMap.empty else case IntMap.lookupGE (2*x) m of
          Nothing -> m
          Just (!x', _) -> go x' (IntMap.update (\x -> if x == 1 then Nothing else Just (x-1)) x' m)

fromList = IntMap.fromListWith (+) . flip zip (repeat 1)

readint = fst . fromJust . C.readInt

processinputs k n s
  | k > n = return ()
  | otherwise = let (s1:s2:ss) = s
                    is = map readint (C.words s2)
                in putStrLn ("Case " ++ show k ++ ": " ++ show (iter . fromList $ is)) >> processinputs (succ k) n ss

processall (C.lines -> (nt:inputs)) = processinputs 1 (readint nt) inputs
main = C.getContents >>= processall
