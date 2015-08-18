{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import Data.IntMap.Strict(IntMap)
import Data.Maybe

pushBack x q = q Seq.|> x
popFront q = (x, q')
  where (!x Seq.:< q') = Seq.viewl q

increaseKey k = snd . IntMap.insertLookupWithKey (\k new old -> succ old) k 1
decreaseKey k = IntMap.updateWithKey (\k v -> if v == 1 then Nothing else Just $! (pred v)) k 

data ArraySub = Empty | ArraySub {
    _size :: {-# UNPACK #-} !Int
  , _queue :: Seq Int
  , _heap :: IntMap Int
  } deriving Show

size Empty = 0
size (ArraySub k _ _) = k

push x Empty = ArraySub 1 (Seq.singleton x) (IntMap.singleton x 1)
push x (ArraySub k q h) = ArraySub (succ k) (pushBack x q) (increaseKey x h)

pushpop x (ArraySub k q h) = ArraySub k q'' h'
    where (!a, !q') = popFront q
          !h' = increaseKey x . decreaseKey a $ h
          !q'' = pushBack x q'

arrayMax (ArraySub k q h) = fst . IntMap.findMax $ h

pushUntil k xxs a
  | k == 0 = (a, xxs)
  | otherwise = pushUntil (pred k) (tail xxs) (push (head xxs) a)

arraysub ints k = go ( Seq.singleton (arrayMax arr) ) arr ints'
  where (arr, ints') = pushUntil k ints Empty
        go r u [] = r
        go r u (x:xs) = go (r Seq.|> arrayMax u') u' xs
          where u' = pushpop x u

pr :: [Int] -> String
pr = foldr p1 ""
  where p1 x = shows x . showString " "

readint = fst . fromJust . C.readInt

process inputs = putStrLn . pr . F.toList $ ans
  where (_:ints) = map readint (C.words inputs)
        !ans = arraysub (init ints) (last ints)

main = C.getContents >>= process
