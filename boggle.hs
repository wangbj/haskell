{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Map(Map)
import Data.HashMap.Strict(HashMap)
import Data.Maybe
import Control.Monad

player :: C.ByteString -> HashMap C.ByteString Int
player s = HashMap.fromListWith (+)  $ zip (C.words s) (repeat 1)

merge :: [HashMap C.ByteString Int] -> HashMap C.ByteString Int
merge = HashMap.filter (> 1) . foldr go HashMap.empty
  where go m r = HashMap.foldrWithKey go1 r m
          where go1 k v r = HashMap.insertWith (+) k 1 r

sc k
  | k <= 4 = 1
  | k == 5 = 2
  | k == 6 = 3
  | k == 7 = 5
  | k > 7 = 11

score m = HashMap.foldrWithKey go 0
  where go k v r
          | HashMap.member k m = r
          | otherwise = r + ((*v) . sc . C.length $ k)

winner txts = maximum . map (score dict) $ ps
  where !ps = map player txts
        !dict = merge ps

process = winner . tail . C.lines

main = C.getContents >>= print . process
