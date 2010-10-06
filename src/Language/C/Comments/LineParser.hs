module Language.C.Comments.LineParser (parseLines) where

import Data.List

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace _ = False

stripLeadingWhitespace = dropWhile isWhitespace
stripTrailingWhitespace = reverse . stripLeadingWhitespace . reverse
dropLast = reverse . tail . reverse
endsWith = \c -> (startsWith c) . reverse

startsWith :: Eq a => a -> [a] -> Bool
startsWith _ [] = False
startsWith x (y:_) = x == y

strip :: String -> String
strip s = if isBrokenLine s then dropLast (stripTrailingWhitespace s) else s
  
isBrokenLine :: String -> Bool
isBrokenLine = (endsWith '\\') . stripTrailingWhitespace

joinBrokenLines :: [String] -> [String]
joinBrokenLines [] = []
joinBrokenLines [line] = [strip line]
joinBrokenLines (line1:line2:rest) =
  if isBrokenLine line1 then 
    let joinedLine = (strip line1) ++ line2 in
    joinBrokenLines (joinedLine:rest)
  else 
    line1:(joinBrokenLines (line2:rest))

parseLines :: String -> [String]
parseLines = joinBrokenLines . lines
