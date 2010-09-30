{
module Language.C.System.Comments 
( comments
, Comment
, commentPosition
, commentText
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

-- | Takes a string containing C code and separates it into the code stripped
-- of comments, and a list of the comments.
extractComments :: String -> (String,[(AlexPosn,String)])
extractComments = 
  alexScanTokens >>> unzip >>> (concat *** filter (not . null . snd))

data Comment = Comment 
  { commentPosition :: Position
  , commentText :: String
  } deriving (Eq,Show)

-- Note that CPP strips comments *after* joining lines marked with \, which 
-- can cause problems with one-line comments.  This is probably a little-used 
-- "feature" in C files, but I should fix it anyway.
commentsForFile :: FilePath -> String -> [Comment]
commentsForFile file code = 
  let 
    convertPosnsIn = map (first (convertPosn file))
    makeComment = \(p,t) -> Comment p t
    (_,cmnts) = extractComments code
  in
    map makeComment (convertPosnsIn cmnts)

comments :: FilePath -> IO [Comment]
comments file = do
  code <- readFile file
  return $ commentsForFile file code

}
