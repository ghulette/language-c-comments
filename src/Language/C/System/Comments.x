{
module Comments (extractComments) where

import Control.Arrow
}

%wrapper "basic"

-- We have to recognize strings, so we don't grab comments inside them.
-- Just copypasted from src/Language/C/Parser/Lexer.x
$octdigit = 0-7
$hexdigit = [0-9a-fA-F]
@charesc  = \\([ntvbrfaeE\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)
$instr    = \0-\255 # [ \\ \" \n \r ] -- valid character in a string literal
@string   = \"($instr|@charesc)*\"

-- Regexp for C comments, thanks to http://ostermiller.org/findcomment.html
@comment  = \/\*([^\*]|[\r\n]|(\*+([^\*\/]|[\r\n])))*\*+\/|(\/\/.*)

tokens :-
  @string  { \s -> (s,"") }
  @comment { \s -> (" ",s) }
  .|\n     { \s -> (s,"") }

{

-- | Takes a C file and separates it into the code stripped of comments, and 
-- a list of the comments.
extractComments :: String -> (String,[String])
extractComments = 
  alexScanTokens >>> unzip >>> (concat *** filter (not . null))

}
