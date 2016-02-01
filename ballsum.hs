import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Ratio

ballsum n k
  | k < 3 = 0
  | otherwise = (cnt k) % (n*(n-1))
  where cnt n
          | even n = n * (n' -1)
          | odd n = (n-1) * n'
          where n' = n `div` 2
ballsum_ n k = ballsum (fromIntegral n) (fromIntegral k)

pr :: (Integral a, Show a) => [Ratio a] -> String
pr = foldr p1 ""
  where p1 x
          | x == 0 = shows 0 . showString "\n"
          | otherwise = shows (numerator x) . showString "/" . shows (denominator x) . showString "\n"

readpair s = fromJust $ C.readInt s >>= \(x, s1) ->
  ( C.readInt . C.tail ) s1 >>= \(y, _) -> return $! (x, y)

processall = putStr . pr . map (uncurry ballsum_ . readpair)
main = C.getContents >>= processall . init . C.lines
