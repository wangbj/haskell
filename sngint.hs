import qualified Data.ByteString.Char8 as C
import Data.Maybe(fromJust)
import Control.Applicative
import Data.List(sort)

divs n
  | n < 10 = Just [n]
  | n `mod` 9 == 0 = (:) <$> (Just 9) <*> (divs (n`div`9))
  | n `mod` 8 == 0 = (:) <$> (Just 8) <*> (divs (n`div`8))
  | n `mod` 7 == 0 = (:) <$> (Just 7) <*> (divs (n`div`7))
  | n `mod` 6 == 0 = (:) <$> (Just 6) <*> (divs (n`div`6))
  | n `mod` 5 == 0 = (:) <$> (Just 5) <*> (divs (n`div`5))
  | n `mod` 4 == 0 = (:) <$> (Just 4) <*> (divs (n`div`4))
  | n `mod` 3 == 0 = (:) <$> (Just 3) <*> (divs (n`div`3))
  | n `mod` 2 == 0 = (:) <$> (Just 2) <*> (divs (n`div`2))
  | otherwise = Nothing

divs' x
  | x == 0 = Just [1, 0]
  | otherwise = fmap sort . divs $ x

p Nothing = "-1\n"
p (Just s) = foldr p1 "\n" s
  where p1 x = shows x
        p1 :: Int -> ShowS

main = C.getContents >>= mapM_ (putStr . p . divs' . readint) . tail . C.lines
  where readint = fst . fromJust . C.readInt
