-- Simple example reads a C file, extracts comments, and prints them.
-- Build with something like this:
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
      fmt  = show $ commentFormat c
  printf "(%d:%d) %s %s\n" row col file fmt
  putStrLn $ commentText c
  putStrLn $ commentTextWithoutMarks c
  putStrLn "---"

main :: IO ()
main = do
  putStrLn $ show SingleLine
  [file] <- getArgs
  cmnts <- comments file
  mapM_ printComment cmnts
