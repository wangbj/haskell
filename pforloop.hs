{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet as S
import Data.Char
import Data.Maybe

ints :: C.ByteString -> [Int]
ints s = S.toList . maybe r2 (flip S.insert r2) $ r1
  where go (!r, !s) c
          | isDigit c = (Just $! maybe (ord c - ord '0') (\x -> x*10 + ord c - ord '0') r, s)
          | otherwise = (Nothing, case r of
                            Nothing -> s
                            Just !x -> S.insert x s)
        (r1, r2) = C.foldl go (Nothing, S.empty) s

pr :: Int -> Int -> IO ()
pr x y = putStrLn $ "for (int i = " ++ show x ++ "; i <= " ++ show (x+y-1) ++ "; i++) cout << i << \" \";"

iter :: [Int] -> IO ()
iter [] = return ()
iter (x:xs) = go 1 x xs
  where go !k p [] = pr p k
        go !k p (c:cs)
          | c == k + p = go (succ k) p cs
          | otherwise = pr p k >> go 1 c cs

main = C.getContents >>= iter . ints
