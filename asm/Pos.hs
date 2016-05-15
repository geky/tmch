module Pos (FilePath, module Pos) where

import Prelude hiding (error)
import Result


-- Position in a file
data Pos = Pos FilePath Line
  deriving (Eq, Show, Ord)

type Line = Int

instance Monoid Pos where
    mempty = Pos "" 0
    mappend = const
    
startOfFile :: FilePath -> Pos
startOfFile fp = Pos fp 1

endOfFile :: FilePath -> Pos
endOfFile fp = Pos fp 0

nextPos :: Pos -> Char -> Pos
nextPos (Pos fp l) '\n' = Pos fp (l+1)
nextPos p _             = p

position :: FilePath -> String -> [(Pos, Char)]
position fp s = zip positions s
  where positions = scanl nextPos (startOfFile fp) s

showPos :: Pos -> String
showPos = \case 
    Pos "" 0 -> "somewhere"
    Pos fp 0 -> "in " ++ fp
    Pos "" l -> "line " ++ show l
    Pos fp l -> "in " ++ fp ++ " line " ++ show l

-- Positional messages
data Msg = Msg Pos String
    deriving Show

msg :: Pos -> String -> Msg
msg = Msg

showMsg :: String -> Msg -> String
showMsg prefix (Msg p m) = unlines
    $ header p
    : map (replicate 4 ' ' ++) (lines m)
  where
    header = \case
        Pos "" 0 -> prefix ++ ":"
        p        -> prefix ++ " " ++ showPos p ++ ":"


-- Result overloading
okMsg :: b -> Result Msg b
okMsg = ok

warningMsg :: Pos -> String -> b -> Result Msg b
warningMsg p m b = warning (msg p m) b

errorMsg :: Pos -> String -> Result Msg b
errorMsg p m = error (msg p m)

resultMsg :: Result Msg a -> Result String a
resultMsg = \case
    Ok a         -> ok a
    Warning es a -> Warning (map (showMsg "warning") es) a
    Error es     -> Error (map (showMsg "error") es)
    
checkMsg :: Result Msg a -> IO a
checkMsg = check . resultMsg

unexpected :: Show a => FilePath -> [(Pos, a)] -> Result Msg b
unexpected fp []       = errorMsg (endOfFile fp) ("unexpected end of input")
unexpected _ ((p,a):_) = errorMsg p              ("unexpected " ++ show a)

expect :: Show a => FilePath -> Either [(Pos, a)] b -> Result Msg b
expect fp = either (unexpected fp) okMsg

