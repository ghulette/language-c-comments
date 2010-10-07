-- Simple example reads a C file, extracts comments, and prints them.
-- Build with something like:
-- ghc -package-conf ../dist/package.conf.inplace --make Main.hs

import Language.C
import Language.C.Comments
import System (getArgs)
import Text.Printf

printComment :: Comment -> IO ()
printComment c = do
  let posn = commentPosition c
      file = posFile posn
      row  = posRow posn
      col  = posColumn posn
  printf "(%d:%d) %s\n" row col file
  putStrLn $ commentText c
  putStrLn "---"

main :: IO ()
main = do
  [file] <- getArgs
  cmnts <- comments file
  mapM_ printComment cmnts
