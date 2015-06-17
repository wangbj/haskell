{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
--import Data.ByteString.Builder
--import Data.ByteString.Builder.ASCII
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
                v1' <- MV.read m s'
                if (s < e && v1' < 9) then do
                  if s' < e' then MV.write m e v1 else MV.write m s (succ v1) >> MV.write m e (succ v1)
                  else do
                  MV.write m s v2
                go m (succ s) (pred e)
        go :: PalinM -> Int -> Int -> IO ()

c1 = C.pack "2134"
            
nextPalinMut :: C.ByteString -> IO Palin
nextPalinMut c = do
  let v1 = fromBS c
  v2 <- V.unsafeThaw v1 :: IO PalinM
  nextPalinM v2
  v3 <- V.unsafeFreeze v2 :: IO Palin
  return v3

doNextPalin_ :: C.ByteString -> IO ()
doNextPalin_ s = nextPalinMut s >>= \r ->
  (putStrLn . V.toList . V.map (\x -> chr (x + ord '0'))) r

getinputs s = C.lines rest
  where (_, t1) = C.break (== '\n') s
        (_, rest) = C.span (== '\n') t1

process = mapM_ doNextPalin_ . getinputs

main = C.getContents >>= process
