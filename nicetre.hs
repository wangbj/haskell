import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.ReadP

leaf = do
  char 'l'
  return 0
  
branch = do
  char 'n'
  x <- preorder
  y <- preorder
  return $! 1 + max x y
  
preorder = leaf +++ branch

valid = do
  d <- preorder
  eof
  return d

maxDepth = fst . head . readP_to_S valid 

main = C.getContents >>= mapM_ (print . maxDepth . C.unpack) . (tail . C.lines)
