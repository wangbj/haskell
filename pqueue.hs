{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import Data.IntMap.Strict(IntMap)
import Data.Sequence(Seq, ViewL(..), (|>))
import Data.Maybe
import Data.Monoid

remove p x m@(IntMap.lookup p -> Nothing) = m
remove p x m@(IntMap.lookup p -> Just vs) =
  let vs' = Seq.filter (/= x) vs in if
    | Seq.null vs'         -> IntMap.delete p m
    | (not . Seq.null) vs' -> IntMap.insert p vs' . IntMap.delete p $ m

pq :: Int -> (Seq (Int, Int),  IntMap (Seq Int)) -> IO ()
pq x = print . uncurry (go 1)
  where
    go r (Seq.viewl -> EmptyL) _ = r
    go r (Seq.viewl -> (p, y) :< q) m@(IntMap.lookupGT p -> Nothing)
      | y == x = r
      | y /= x = go (succ r) q (remove p y m)
    go r (Seq.viewl -> py@(p, y) :< q) m@(IntMap.lookupGT p -> Just (k, v)) =
      go (r) (q |> py) m
    
process l1 l2 = pq (succ i) (Seq.fromList (zip (i2++(repeat 1)) [1..n]), (IntMap.fromListWith (flip (<>)) is))
  where (n:i:_) = map readint (C.words l1)
        i2 = map readint (C.words l2)
        is = zip (i2 ++ (repeat 1)) (map Seq.singleton [1..n])

processinputs 0 _ = return ()
processinputs n (s1:s2:ss) = process s1 s2 >> processinputs (n-1) ss

readint = fst . fromJust . C.readInt
main = fmap readint C.getLine >>= \n -> C.getContents >>= processinputs n . C.lines
