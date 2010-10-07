module Language.C.Comments
  ( Comment
  , commentPosition
  , commentText
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

parseComments :: String -> (String,[(AlexPosn,String)])
parseComments = 
  alexScanTokens >>> unzip >>> (concat *** concatMap maybeToList)

-- | Comment positions use Language.C.Data.Position for compatibility with
-- Language.C.
data Comment = Comment 
  { commentPosition :: Position
  , commentText :: String
  } deriving (Eq,Show)

-- | Comments are ordered by position within files.
instance Ord Comment where
  compare x y = compare (commentPosition x) (commentPosition y)

commentsInFile :: FilePath -> String -> [Comment]
commentsInFile file code = 
  let 
    convertPosnsIn = map (first (convertPosn file))
    makeComment = \(p,t) -> Comment p t
    joinBrokenLines = unlines . parseLines
    (_,cmnts) = parseComments (joinBrokenLines code)
  in
    map makeComment (convertPosnsIn cmnts)

-- | Extract comments from a C file
comments :: FilePath -> IO [Comment]
comments file = do
  code <- readFile file
  return $ commentsInFile file code

-- | Extract comments from a string.  A comment's position contains a
-- filename; this method uses the empty string in its place.
commentsFromString :: String -> [Comment]
commentsFromString = commentsInFile ""
