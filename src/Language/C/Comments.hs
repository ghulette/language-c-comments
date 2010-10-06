module Language.C.Comments
( comments
, Comment
, commentPosition
, commentText
) 
where
  
import Control.Arrow
import Language.C.Comments.Lexer
import Language.C.Comments.LineParser
import Language.C.Data.Position
import System.IO

convertPosn :: FilePath -> AlexPosn -> Position
convertPosn file (AlexPn offset line col) = Position file line col

extractComments :: String -> (String,[(AlexPosn,String)])
extractComments = 
  alexScanTokens >>> unzip >>> (concat *** filter (not . null . snd))

data Comment = Comment 
  { commentPosition :: Position
  , commentText :: String
  } deriving (Eq,Show)

commentsForFile :: FilePath -> String -> [Comment]
commentsForFile file code = 
  let 
    convertPosnsIn = map (first (convertPosn file))
    makeComment = \(p,t) -> Comment p t
    (_,cmnts) = extractComments code
  in
    map makeComment (convertPosnsIn cmnts)

-- | Extract comments from a C file
comments :: FilePath -> IO [Comment]
comments file = do
  code <- readFile file
  let ls = parseLines code -- parseLines joins broken lines
  return $ commentsForFile file (unlines ls)
