module Language.C.Comments
  ( Comment
  , CommentFormat(SingleLine,MultiLine)
  , commentPosition
  , commentText
  , commentTextWithoutMarks
  , commentFormat
  , comments
  , commentsFromString
  ) where
  
import Control.Arrow
import Data.Maybe (maybeToList)
import Language.C.Comments.Lexer
import Language.C.Comments.LineParser (parseLines)
import Language.C.Data.Position

convertPosn :: FilePath -> AlexPosn -> Position
convertPosn file (AlexPn offset line col) = Position file line col

parseComments :: String -> (String,[(AlexPosn,String,CommentFormat)])
parseComments = 
  alexScanTokens >>> unzip >>> (concat *** concatMap maybeToList)

-- | Comment positions use Language.C.Data.Position for compatibility with
-- Language.C.
data Comment = Comment {
  -- | The position of the comment within the source file.
  commentPosition :: Position,
  -- | The text of a comment (including the comment marks).
  commentText :: String,
  -- | The format of a comment (single- or multi-line).
  commentFormat :: CommentFormat
} deriving (Eq,Show)
  
-- | The text of a comment, but with the comment marks removed.
commentTextWithoutMarks :: Comment -> String
commentTextWithoutMarks c = stripCommentMarks fmt (commentText c)
  where fmt = commentFormat c

-- | Comments are ordered by position within files.
instance Ord Comment where
  compare x y = compare (commentPosition x) (commentPosition y)

stripCommentMarks :: CommentFormat -> String -> String
stripCommentMarks SingleLine = drop 2
stripCommentMarks MultiLine = reverse . drop 2 . reverse . drop 2

makeComment :: FilePath -> (AlexPosn,String,CommentFormat) -> Comment
makeComment file (pos,txt,fmt) = Comment pos' txt fmt
  where pos' = convertPosn file pos

commentsInFile :: FilePath -> String -> [Comment]
commentsInFile file code = map (makeComment file) cmnts
  where joinBrokenLines = unlines . parseLines
        (_,cmnts) = parseComments (joinBrokenLines code)

-- | Extract comments from a C file.
comments :: FilePath -> IO [Comment]
comments file = do
  code <- readFile file
  return $ commentsInFile file code

-- | Extract comments from a string.  A comment's position contains a
-- filename; this method uses the empty string in its place.
commentsFromString :: String -> [Comment]
commentsFromString = commentsInFile ""
