module Pos (FilePath, module Pos) where

import Prelude hiding (error)
import Result
import Data.List
import Data.Bifunctor


-- Position in a file
data Pos = Pos FilePath !Line !Col
  deriving (Eq, Show, Ord)

type Line = Int
type Col  = Int

instance Monoid Pos where
    mempty = startOfFile ""
    mappend = const
    
startOfFile :: FilePath -> Pos
startOfFile fp = Pos fp 1 1

endOfFile :: FilePath -> Pos
endOfFile fp = Pos fp 0 0

nextPos :: Pos -> Char -> Pos
nextPos (Pos fp l c) = \case
    '\n' -> Pos fp (l+1) 1
    _    -> Pos fp l (c+1)

position :: FilePath -> String -> [(Pos, Char)]
position fp s = zip positions s
  where positions = scanl nextPos (startOfFile fp) s

showPos :: Pos -> String
showPos = intercalate ":" . \case 
    Pos "" 0 0 -> []
    Pos fp 0 0 -> [fp]
    Pos fp l 0 -> [showPos (Pos fp 0 0), show l]
    Pos fp l c -> [showPos (Pos fp 0 0), show l, show c]

-- Positional messages
data Msg = Msg Pos String String
    deriving Show

showMsg :: Msg -> String
showMsg (Msg p t m) = unlines
    $ header
    : map (replicate 4 ' ' ++) (lines m)
  where
    header = case t of
        "" -> showPos p ++ ":"
        _  -> showPos p ++ ": " ++ t ++ ":"


-- Result overloading
okMsg :: b -> Result Msg b
okMsg = ok

warningMsg :: Pos -> String -> b -> Result Msg b
warningMsg p m b = warning (Msg p "warning" m) b

errorMsg :: Pos -> String -> Result Msg b
errorMsg p m = error (Msg p "error" m)

resultMsg :: Pos -> Result String a -> Result Msg a
resultMsg p = \case
    Ok a         -> Ok a
    Warning es a -> Warning (map (Msg p "warning") es) a
    Error es     -> Error (map (Msg p "error") es)
    
checkMsg :: Result Msg a -> IO a
checkMsg = check . first showMsg


unexpected :: Show a => FilePath -> [(Pos, a)] -> Result Msg b
unexpected fp []       = errorMsg (endOfFile fp) ("unexpected end of input")
unexpected _ ((p,a):_) = errorMsg p              ("unexpected " ++ show a)

expect :: Show a => FilePath -> Either [(Pos, a)] b -> Result Msg b
expect fp = either (unexpected fp) okMsg

