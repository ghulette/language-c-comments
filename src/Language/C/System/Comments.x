{
module Language.C.System.Comments 
( extractComments
, comments
) 
where
import Control.Arrow
import Language.C.Data.Position
import System.IO
}

%wrapper "posn"

-- We have to recognize strings, so we don't grab comments inside them.
-- Just copypasted from src/Language/C/Parser/Lexer.x
$octdigit = 0-7
$hexdigit = [0-9a-fA-F]
@charesc  = \\([ntvbrfaeE\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)
$instr    = \0-\255 # [ \\ \" \n \r ] -- valid character in a string literal
@string   = \"($instr|@charesc)*\"

-- Regexp for C comments, credit to http://ostermiller.org/findcomment.html
@comment  = \/\*([^\*]|[\r\n]|(\*+([^\*\/]|[\r\n])))*\*+\/|(\/\/.*)

tokens :-
  @string  { \pos s -> (s,(pos,"")) }
  @comment { \pos s -> (" ",(pos,s)) }
  .|\n     { \pos s -> (s,(pos,"")) }

{

-- | Convert an Alex position to a C Language position
convertPosn :: FilePath -> AlexPosn -> Position
convertPosn file (AlexPn offset line col) = Position file line col

-- | Takes a C file and separates it into the code stripped of comments, and 
-- a list of the comments.
extractComments :: String -> (String,[(AlexPosn,String)])
extractComments = 
  alexScanTokens >>> unzip >>> (concat *** filter (not . null . snd))

comments :: FilePath -> IO [(Position,String)]
comments file = do
  code <- readFile file
  let (_,cmnts) = extractComments code
  return $ convertPosnsIn cmnts
  where
    convertPosnsIn = map (first (convertPosn file))
}
