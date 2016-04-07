import qualified Data.ByteString.Char8 as C
import Data.Foldable(foldMap)
import Data.Array.Unboxed
import Data.Maybe
import Data.Monoid

getinputs :: C.ByteString -> [UArray Int Int]
getinputs = map get1 . tail . C.lines
  where
    readint = fst . fromJust . C.readInt
    get1 s  = listArray (1, n) (scanl1 (+) s') :: UArray Int Int
          where (n:s') = map readint (C.words s)

sumUpto   u i   | i  < 1 = 0
                | i >= 1 = u ! i
sumFromto u i j | i  > j = 0
                | i == 1 = sumUpto u j
                | i  > 1 = sumUpto u j - sumUpto u (i-1)

newtype Min a = Min {
    getMin :: a
  } deriving (Show, Eq, Ord)

instance (Bounded a, Ord a) => Monoid (Min a) where
  mempty = Min maxBound
  Min x `mappend` Min y = Min (min x y)

choose u n i = Min (min l r)
  where l = 2 * (sumUpto u (pred i)) + sumFromto u (succ i) n
        r = 2 * (sumFromto u (succ i) n) + sumUpto u (pred i)

cir u = getMin . foldMap (choose u n) $ [1..n]
  where (_, n) = bounds u

main = C.getContents >>= mapM_ (print . cir) . getinputs
