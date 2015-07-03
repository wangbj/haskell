{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Builder
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707
import Data.ByteString.Lazy.Builder.ASCII
#endif
import Data.Array.Unboxed
import Data.Array.IO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import Data.Array.Unsafe
#endif
import Data.Maybe (fromJust)

ans = scanl (+) 2 [1,2,2,4,2,4,6,2,4,8,4,2,4,6,8,6,4,6,6,6,2,6,10,8,4,2,6,4,12,6,8,4,12,2,4,2,12,16,2,10,2,4,6,2,4,8,10,8,12,6,4,14,6,6,16,2,6,4,12,6,2,16,6,6,8,10,8,4,2,10,2,4,8,18,4,8,6,12,4,6,6,8,10,8,6,12,12,12,4,12,2,12,6,4,8,18,4,6,6,8,6,12,6,6,6,4,20,6,16,2,6,18,12,4,6,6,2,6,24,4,2,10,2,4,6,2,16,12,20,16,2,6,4,2,16,2,6,6,18,6,10,8,6,22,2,4,12,2,30,4,6,2,6,12,10,8,6,4,26,16,8,6,10,6,2,4,2,6,10,8,16,6,6,14,6,10,2,18,18,10,2,4,8,4,2,10,12,24,8,4,2,24,16,6,6,14,4,6,8,6,6,4,12,6,8,10,12,12,26,4,12,8,6,10,8,4,12,14,4,8,4,2,12,22,12,8,4,14,4,12,2,10,6,2,10,6,6,6,18,14,10,6,8,4,12,12,18,12,14,6,4,8,12,6,12,4,8,16,2,18,10,2,16,12,6,26,6,6,22,8,6,4,8,4,24,8,6,10,12,2,16,6,30,18,2,4,6,8,6,4,2,10,18,6,8,4,14,12,4,8,10,8,4,14,4,12,12,2,4,8,28,18,12,8,10,14,6,10,12,6,8,4,6,8,12,16,2,4,2,12,4,8,22,8,18,4,6,24,8,12,4,12,6,12,12,2,22,6,6,18,6,6,12,2,18,6,4,6,20,18,12,16,2,12,4,6,12,2,12,18,10,12,18,2,28,2,4,12,8,4,2,16,2,22,6,2,10,12,14,6,12,4,2,12,12,18,10,2,18,16,30,2,6,16,2,4,8,18,4,8,6,12,6,12,10,2,4,12,12,2,6,28,2,10,2,16,8,24,10,12,2,6,6,18,4,14,4,20,16,14,4,12,14,4,2,18,10,12,2,6,6,4,32,4,8,4,24,18,2,4,8,28,8,24,4,2,6,4,2,10,8,24,22,6,6,12,2,24,16,2,6,16,12,12,2,4,14,10,30,14,6,16,8,4,14,4,6,18,6,6,24,6,6,18,8,4,12,2,28,18,2,16,8,6,12,4,8,18,4,14,10,2,4,12,8,4,6,12,14,40,8,12,4,24,6,6,14,6,10,14,4,6,6,20,12,4,8,4,12,8,22,2,18,4,8,4,2,16,12,8,6,10,30,6,8,10,20,4,2,10,14,16,8,12,12,16,2,10,6,8,4,20,12,16,14,4,8,6,10,14,6,16,14,22,6,2,10,2,28,2,18,10,2,4,12,12,2,12,6,12,34,6,2,18,4,6,6,2,22,6,24,2,16,12,6,12,18,2,10,14,4,18,6,6,2,6,10,6,30,6,8,16,2,12,16,6,18,8,4,12,12,36,2,6,10,12,14,4,6,2,12,10,8,16,12,6,2,6,6,40,14,30,6,10,2,4,12,26,6,10,2,18,30,12,6,10,2,12,4,18,6,6,8,16,6,18,6,6,18,6,14,6,4,6,24,12,2,12,12,4,26,6,12,10,2,6,22,14,4,26,10,6,6,14,30,12,12,24,6,6,4,6,6,2,12,4,6,14,16,8,16,24,6,12,2,10,2,10,2,12,22,14,4,12,12,6,6,6,8,6,18,10,2,10,8,6,22,2,10,18,2,10,6,20,22,12,18,12,20,6,16,6,2,10,14,18,12,4,12,14,30,4,12,14,4,8,4,2,4,14,28,8,18,6,22,14,16,8,22,2,16,2,12,4,12,6,20,6,4,12,12,2,12,12,16,14,10,6,2,22,6,20,10,6,8,6,4,8,18,4,26,16,2,10,12,6,2,22,2,28,2,16,2,12,12,4,14,16,2,30,24,4,18,2,16,6,2,22,6,18,14,22,6,14,4,2,16,2,6,4,6,24,12,2,4,8,30,16,24,6,2,4,12,6,20,6,30,16,2,10,2,10,2,28,38,10,6,6,2,16,6,20,4,6,12,8,4,2,24,4,14,12,16,6,14,4,14,12,16,12,6,8,12,10,14,10,18,8,4,14,4,2,10,6,8,6,16,14,12,10,14,4,6,54,6,8,6,6,16,2,4,32,28,18,8,4,8,6,10,20,24,4,6,8,12,22,2,16,6,2,28,6,18,8,4,8,6,12,12,24,30,22,2,12,16,6,2,6,16,6,12,2,16,8,4,2,24,24,12,4,8,4,8,16,14,10,8,4,2,6,18,22,8,18,4,6,18,6,20,4,2,4,12,24,8,10,12,8,6,10,8,28,2,16,20,12,4,6,6,12,6,12,2,10,12,14,30,16,18,2,24,6,28,2,4,26,12,4,14,10,14,10,6,8,16,6,20,24,6,4,6,2,28,14,10,12,14,16,2,10,18,14,4,14,16,6,14,6,12,18,16,2,6,24,10,2,10,6,2,22,14,6,30,16,12,20,6,4,2,10,14,6,16,2,6,4,20,6,4,12,2,6,22,36,14,4,30,6,6,2,10,6,20,6,10,6,8,10,12,8,6,10,2,4,2,12,28,12,14,10,12,12,6,12,2,6,16,6,32,30,10,6,6,2,28,14,4,20,6,4,18,2,28,2,6,30,10,12,2,28,8,4,2,30,16,2,12,18,10,2,10,2,6,6,18,22,6,20,4,8,18,4,8,6,12,4,20,10,12,2,10,2,34,6,6,30,2,22,18,2,4,14,12,16,12,2,40,6,8,4,12,14,6,10,2,6,36,4,2,10,8,12,4,20,6,16,26,4,56,6,12,4,6,18,2,18,6,16,20,4,8,6,6,4,48,2,24,16,6,8,18,4,14,10,12,6,2,10,14,18,10,2,4,8,4,26,18,4,2,16,8,4,12,26,4,12,8,6,30,4,2,10,2,34,2,12,18,16,36,8,6,4,2,24,4,6,8,6,12,12,6,6,10,30,24,30,2,4,6,2,12,16,14,6,4,8,18,10,2,4,2,10,42,2,6,12,4,6,2,42,22,8,10,20,6,10,2,16,18,8,22,2,10,2,10,2,4,14,4,26,10,18,2,28,18,2,28,8,22,26,4,8,6,4,14,6,16,18,2,12,12,4,12,8,42,10,2,4,14,10,6,14,24,4,2,42,6,12,24,16,8,6,6,10,6,2,10,12,6,12,30,6,24,2,16,8,22,2,6,10,6,20,16,30,8,4,2,12,22,14,6,30,22,8,18,22,6,8,10,2,4,6,2,12,4,14,12,6,4,6,18,6,20,4,12,6,2,22,24,12,2,24,12,16,2,6,12,10,36,12,14,16,12,14,16,2,4,2,16,12,26,16,6,14,6,6,18,10,2,16,18,6,6,2,16,12,12,6,6,14,18,6,12,10,14,4,2,4,8,30,28,2,4,24,2,18,4,12,8,10,24,6,6,20,12,4,26,22,12,32,12,10,18,6,6,8,12,16,2,6,18,4,24,2,16,32,4,8,18,4,8,12,6,4,6,8,54,6,36,10,6,2,4,8,6,10,18,2,4,20,6,12,12,6,18,6,4,6,14,30,6,16,20,4,6,2,4,12,32,4,14,10,18,2,30,6,6,12,6,22,6,2,6,22,26,16,2,10,8,10,18,30,30,2,16,6,2,12,6,10,20,30,4,6,8,28,8,4,2,4,24,18,2,6,16,14,16,2,6,10,2,4,8,4,44,4,26,12,4,8,24,30,16,8,4,2,16,6,18,6,20,4,12,8,12,4,12,26,4,2,6,4,6,6,24,8,4,30,8,4,2,4,20,10,12,2,6,6,24,22,12,6,14,6,24,10,2,4,30,8,12,6,4,42,8,34,18,2,12,4,14,12,16,12,6,6,2,22,12,18,20,18,16,6,8,18,18,16,24,12,18,6,14,6,4,20,6,16,6,8,16,8,12,16,18,30,30,6,14,4,18,8,4,26,4,12,2,22,8,4,12,2,16,24,2,6,12,24,10,8,18,10,6,12,14,22,6,6,12,2,12,16,14,12,4,2,4,14,28,2,22,8,10,14,12,6,12,16,6,26,4,6,18,2,4,14,24,4,14,10,2,6,16,36,2,16,12,2,16,2,12,16,6,6,8,6,4,30,2,18,22,2,6,16,8,16,6,8,18,10,8,6,6,4,12,18,14,10,2,28,18,2,22,6,12,12,2,12,18,6,12,28,14,4,12,2,40,30,18,26,4,2,4,6,20,4,14,6,4,26,46,2,4,8,10,6,20,28,14,16,12,6,8,30,4,2,12,4,18,8,6,16,30,20,4,12,2,42,4,2,4,12,8,6,16,2,10,18,8,10,12,24,2,48,18,6,10,8,12,4,2,12,12,10,6,32,4,26,12,10,8,18,28,14,4,2,6,16,2,28,6,8,16,8,12,4,12,8,10,6,8,6,34,2,4,8,6,10,12,14,18,4,2,12,28,18,8,18,6,6,6,10,8,12,30,28,26,18,4,14,16,12,2,10,14,6,42,6,12,10,6,12,20,12,12,4,12,12,6,48,6,6,18,12,2,6,6,10,6,8,18,4,24,8,12,10,2,12,4,12,20,10,8,10,30,18,12,14,4,2,16,30,50,4,30,2,4,8,4,12,2,24,10,20,10,14,10,14,4,8,18,16,6,6,8,10,2,12,6,30,10,6,6,2,18,34,12,8,10,14,6,6,4,12,18,12,2,10,18,2,10,12,6,2,12,12,16,30,12,36,2,10,6,8,4,8,4,12,8,10,18,2,16,6,8,12,12,4,18,24,2,16,18,8,4,8,4,30,32,12,4,2,24,22,14,10,12,14,10,14,6,4,44,4,26,4,26,4,8,4,42,8,6,10,38,30,6,6,22,12,2,4,8,16,2,4,20,30,24,10,2,12,12,4,6,6,8,6,6,10,2,16,2,10,20,12,6,16,12,14,4,12,2,12,12,22,8,12,16,2,28,6,8,4,12,8,22,8,6,6,4,14,6,16,24,12,14,12,6,22,6,8,18,16,2,10,6,44,10,18,6,12,2,22,6,14,30,4,6,2,16,26,6,12,6,10,2,12,34,8,16,24,6,6,12,2,24,10,26,16,14,16,2,16,8,4,2,6,10,26,12,4,32,6,22,8,22,12,2,12,4,8,10,14,24,4,42,6,8,6,16,6,14,10,12,12,2,12,36,22,20,10,2,22,6,18,2,4,6,24,8,4,2,46,6,6,18,8,4,30,18,6,30,18,20,6,30,18,18,4,6,2,6,12,24,6,24,10,24,6,20,4,2,10,14,24,6,6,4,6,6,12,2,24,6,24,6,4,6,14,6,12,18,10,14,4,32,10,2,6,10,14,18,18,10,2,18,16,8,6,12,12,4,8,10,20,10,8,6,10,14,16,2,18,10,2,10,6,2,6,6,16,2,6,4,26,16,32,30,4,38,4,2,16,14,24,16,8,6,4,6,8,6,6,10,8,28,2,16,6,8,6,30,12,18,12,16,24,6,2,6,10,6,2,6,22,30,18,12,6,8,22,2,40,12,2,6,12,46,8,4,6,14,6,4,2,16,38,22,8,4,12,30,2,16,6,24,2,18,10,6,24,18,8,6,10,18,18,2,6,6,10,6,18,50,10,12,26,12,30,10,6,8,24,18,10,14,16,8,10,2,10,18,14,16,12,2,4,8,4,6,20,22,2,12,4,12,14,10,14,30,4,20,6,4,2,10,30,2,4,48,6,14,10,14,10,2,16,44,10,14,10,8,28,8,10,12,6,12,12,8,6,4,2,18,6,4,6,8,4,26,4,8,36,10,2,6,10,2,12,12,16,2,16,20,12,4,42,18,8,4,2,6,6,18,36,4,26,4,18,12,18,8,4,2,16,2,16,8,4,24,20,24,18,28,6,2,16,8,4,2,12,46,8,16,2,4,24,2,12,6,12,34,12,2,16,12,2,40,20,16,18,6,2,12,16,8,6,10,30,14,6,4,18,12,12,20,4,8,4,2,12,4,24,12,36,2,10,2,4,20,24,10,6,12,14,10,24,6,18,20,10,6,14,30,10,14,4,8,4,12,2,12,24,10,2,10,6,14,18,42,4,12,2,22,2,6,10,6,12,30,18,8,12,6,4,14,6,10,26,24,4,2,28,14,6,22,2,4,14,4,8,42,6,4,6,6,14,16,38,4,6,12,8,22,12,14,18,12,12,10,12,8,4,14,30,4,8,18,22,26,16,8,10,6,8,40,2,4,14,12,12,22,12,14,4,36,6,8,4,14,6,16,14,16,8,36,10,14,4,30,12,6,6,2,12,16,8,12,28,18,12,12,6,30,24,6,44,4,6,2,12,4,2,16,2,6,22,14,10,30,2,6,6,10,24,12,6,12,6,6,14,16,32,18,16,8,10,6,20,4,6,44,10,20,4,18,2,16,20,4,2,10,6,2,30,6,6,4,6,6,2,12,16,8,24,6,16,6,8,22,2,6,4,2,12,4,8,4,18,2,6,34,6,12,8,30,30,18,30,4,2,6,4,12,30,2,16,2,4,8,16,12,2,18,4,30,24,12,2,4,24,14,16,2,34,12,14,16,6,6,8,6,10,14,22,2,12,22,14,18,10,2,4,12,12,6,26,28,2,18,24,4,2,16,14,22,6,26,28,2,6,10,12,18,6,14,22,2,4,24,14,10,42,8,4,2,12,18,4,38,4,24,14,4,6,14,24,4] :: [Int]

mkAns = listArray (1, length ans) ans :: UArray Int Int

readint = fst . fromJust . C.readInt

getinputs = map readint . init . C.lines

assist = (mkAns !)

process = map assist . getinputs

main = C.getContents >>= mapM_ print . process
