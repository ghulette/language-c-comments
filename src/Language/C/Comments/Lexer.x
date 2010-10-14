{
module Language.C.Comments.Lexer where

}

%wrapper "posn"

-- We have to recognize strings, so we don't grab pseudo-comments inside them.
-- Just copypasted from src/Language/C/Parser/Lexer.x
$octdigit = 0-7
$hexdigit = [0-9a-fA-F]
@charesc  = \\([ntvbrfaeE\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)
$instr    = \0-\255 # [ \\ \" \n \r ] -- valid character in a string literal
@string   = \"($instr|@charesc)*\"

-- Regexp for C comments, credit to http://ostermiller.org/findcomment.html
@multiLineComment  = \/\*([^\*]|[\r\n]|(\*+([^\*\/]|[\r\n])))*\*+\/
@singleLineComment = \/\/.*

tokens :-
  @string            { \pos s -> (s  , Nothing) }
  @multiLineComment  { \pos s -> (" ", Just (pos,s,MultiLine)) }
  @singleLineComment { \pos s -> (" ", Just (pos,s,SingleLine)) }
  .|\n               { \pos s -> (s  , Nothing) }

{
-- | Comments can be either single- or multi-line style.
data CommentFormat = SingleLine | MultiLine deriving (Eq,Show)

}
