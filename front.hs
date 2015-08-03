{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Maybe(fromJust)

data P = P {
    _p_x :: {-# UNPACK #-} !Int
  , _p_y :: {-# UNPACK #-} !Int
  }

instance Show P where
  show (P x y) = show (x, y)
  
main = C.getContents >>= print . front . getinputs

getinputsHelper !k !s
  | k == 0 || C.null s = []
  | otherwise = let !(!c, !s') = fromJust $ C.readInt s >>= \(i1, s1) ->
                      (C.readInt . C.tail) s1 >>= \(i2, r) ->
                      return $! (P i1 i2, C.tail r)
                in c: getinputsHelper (pred k) s'
  

getinputs_ s = getinputsHelper (readint h) s'
  where (h, s1) = C.break (== '\n') s
        (_, s') = C.span (== '\n') s1
        readint = fst . fromJust . C.readInt
{-# INLINE getinputs_ #-}

getinputs = foldl go Seq.empty . getinputs_
  where go !s xy@(P x y) = if Seq.null s then Seq.singleton (P x y) else
                            let s' Seq.:> (P x' y') = Seq.viewr s
                            in if x' == x then s' Seq.|> (P x (max y y'))
                               else s Seq.|> xy

front :: Seq.Seq P -> Int
front = _p_y . F.foldr go (P minBound 0)
  where go (P k v) (P p r)
          | v > p = P v (succ r)
          | otherwise = P p r
