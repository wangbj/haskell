{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.Maybe

fromList  :: [ (Int, Int) ] -> IntMap Int
fromList = foldl acc IntMap.empty
  where acc m (!i, !j) = IntMap.insert i 1 . IntMap.insert j (-1) $ m

iter = fst . IntMap.foldlWithKey acc (0, 0)
  where acc (!r, !c) k v = (r', c')
          where c' = c + v
                r' = max r c'

readint = fst . fromJust . C.readInt
readpair s = (x, y)
  where (x:y:_) = map readint (C.words s)

process = iter . fromList

processinputs 0 _ = return ()
processinputs k (n:s) = print (process is) >> processinputs (pred k) s'
  where (s1, s') = splitAt (readint n) s
        is = map readpair s1

processall (nt:s) = processinputs (readint nt) s

main = C.getContents >>= processall . C.lines
