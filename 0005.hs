{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.Primitive
import Control.Monad
import Data.List
import Data.Char

type Palin = V.Vector Int

isPalinS x = reverse x == x

dig x
  | x >= '0' && x <= '9' = ord x - ord '0'

fromBS = V.map dig . V.fromList . C.unpack

isPalin x = go x 0 ( (pred . V.length) x)
  where go v !s !e
          | s > e = True
          | otherwise = case v V.! s == v V.! e of
            True -> go v (succ s) (pred e)
            _ -> False
                                      

nextPalin x = go x 0 ( (pred . V.length) x) False
  where go v !s !e c
          | s >= e = v
          | otherwise = if c then go (v V.// [(s, succ (v V.! s))]) s e False
                        else case compare (v V.! s) (v V.! e) of
                          EQ -> go v (succ s) (pred e) c
                          GT -> go (v V.// [(e, (v V.! e))]) (succ s) (pred e) c
                          LT -> go (v V.// [(e, (v V.! s))]) (succ s) (pred e) True

fromBSM :: C.ByteString -> IO (V.MVector (PrimState IO)  Int)
fromBSM = V.unsafeThaw . fromBS

type PalinM =  V.MVector (PrimState IO) Int

nextPalinM :: PalinM -> IO ()
nextPalinM x = go x 0 (pred . MV.length $ x)
  where go m s e
          | s >= e = return ()
          | otherwise = do
            v1 <- MV.read m s
            v2 <- MV.read m e
            case compare v1 v2 of
              EQ -> go m (succ s) (pred e)
              GT -> MV.write m e v1 >> go m (succ s) (pred e)
              LT -> do
                let s' = succ s
                    e' = pred e
                v3 <- MV.read m s'
                MV.write m e v1
                when (s' < e') ( MV.write m (succ s) (succ v3) )
                go m (succ s) (pred e)
        go :: PalinM -> Int -> Int -> IO ()
            
            
nextPalinMut :: C.ByteString -> IO Palin
nextPalinMut c = do
  let v1 = fromBS c
  v2 <- V.unsafeThaw v1 :: IO PalinM
  nextPalinM v2
  v3 <- V.unsafeFreeze v2 :: IO Palin
  return v3
