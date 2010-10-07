-- Turns a raw C string into lines, taking into account lines that are 
-- explicitly broken with a trailing slash (\).

module Language.C.Comments.LineParser (parseLines) where

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace _ = False

stripTrailingWhitespace :: String -> String
stripTrailingWhitespace = reverse . (dropWhile isWhitespace) . reverse

stripBreak :: String -> String
stripBreak s | isBrokenLine s  = dropLast (stripTrailingWhitespace s)
             | otherwise       = s
  where 
    dropLast = reverse . tail . reverse
  
isBrokenLine :: String -> Bool
isBrokenLine = (endsWith '\\') . stripTrailingWhitespace
  where 
    startsWith _ [] = False
    startsWith x (y:_) = x == y
    endsWith = \c -> (startsWith c) . reverse

joinBrokenLines :: [String] -> [String]
joinBrokenLines [] = []
joinBrokenLines [line] = [stripBreak line] -- if the last line is broken
joinBrokenLines (line1:line2:rest) =
  if isBrokenLine line1 then 
    let joinedLine = (stripBreak line1) ++ line2 in
    joinBrokenLines (joinedLine:rest)
  else 
    line1:(joinBrokenLines (line2:rest))

parseLines :: String -> [String]
parseLines = joinBrokenLines . lines
