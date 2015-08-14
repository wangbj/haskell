import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.Maybe

readint = fst . fromJust . C.readInt

party = IntMap.fromListWith (+) . flip zip (repeat 1) . map readint . C.words

deleteFindMax m = case v of
  1 -> (k, m')
  v -> (k, IntMap.insert k (pred v) m')
  where ( (k, v), m' ) = IntMap.deleteFindMax m

deleteFindMin m = case v of
  1 -> (k, m')
  v -> (k, IntMap.insert k (pred v) m')
  where ( (k, v), m' ) = IntMap.deleteFindMin m

decrease k v m = IntMap.updateWithKey (\_ v -> if v > 1 then Just (pred v) else Nothing) k m

war p1 p2 = go 0 p1 p2
  where go r p q
          | IntMap.null p = r + IntMap.size q
          | IntMap.null q = r
          | otherwise = let (x, p') = deleteFindMax p
                        in case IntMap.lookupGT x q of
                          Nothing -> go r p' (snd . deleteFindMin $ q)
                          Just (k, v) -> go (succ r) p' (decrease k v q)

process txts = war p1 p2
  where (_:l1:l2:_) = C.lines txts
        p1 = party l1
        p2 = party l2

main = C.getContents >>= print . process
