module Parse where

import Prelude hiding (break, return)
import Control.Applicative
import Control.Monad
import Data.Char
import Rule
import Result
import Pos


data Token
    = Label String
    | Ins String [Either String Int]
    | String String
    | Byte Int
  deriving Show


symbol :: Rule Char String
symbol = many1 (matchIf isAlphaNum)

digit :: Int -> Rule Char Int
digit base = fromIntegral . digitToInt <$> matchIf isBase
  where isBase c = isDigit c && digitToInt c < base

ws :: Rule Char String
ws = many1 (matchIf isWs)
  where isWs c = c /= '\n' && isSpace c

nl :: Rule Char String
nl = many ws *> optional comment *> matches "\n"
  where comment = matches ";" *> many (matchIf (/= '\n'))

token :: String -> Rule Char String
token s = optional ws *> matches s <* optional ws

args :: Rule Char [Either String Int]
args = ws *> delimited arg (token ",") <|> pure []
  where
    arg = rule $ \case
        c:_ | isAlpha c -> Left <$> symbol
        c:_ | isDigit c -> Right <$> num
        _               -> reject

op :: Rule Char Token
op = do
    s <- symbol
    rule $ \case
        ':':_ -> accept 1 $ Label s
        _     -> Ins s <$> args <* nl

num :: Rule Char Int
num = do
    sign <- rule $ \case
        '-':_ -> accept 1 negate
        '+':_ -> accept 1 id
        _     -> accept 0 id
    base <- rule $ \case
        '0':b:_ | elem b "bB" -> accept 2 2
        '0':b:_ | elem b "oO" -> accept 2 8
        '0':b:_ | elem b "xX" -> accept 2 16
        _                     -> accept 0 10
    digits <- many1 (digit base)
    return $ sign (foldr1 (\a b -> a*base + b) digits)

string :: Char -> Rule Char String
string q = match q *> many (char q) <* match q
  where
    escape count base = do
        digits <- replicateM count (digit base)
        return $ chr (foldr (\a b -> a*base + b) 0 digits)

    char q = rule $ \case
        '\\':'\\':_  -> accept 2 '\\'
        '\\':'\'':_  -> accept 2 '\''
        '\\':'\"':_  -> accept 2 '\"'
        '\\':'f':_   -> accept 2 '\f'
        '\\':'n':_   -> accept 2 '\n'
        '\\':'r':_   -> accept 2 '\r'
        '\\':'t':_   -> accept 2 '\t'
        '\\':'v':_   -> accept 2 '\v'
        '\\':'0':_   -> accept 2 '\0'
        '\\':'b':_   -> accept 2 () >> escape 8 2
        '\\':'o':_   -> accept 2 () >> escape 3 8
        '\\':'d':_   -> accept 2 () >> escape 3 10
        '\\':'x':_   -> accept 2 () >> escape 2 16
        '\\':_       -> reject
        '\n':_       -> reject
        c:_ | c /= q -> accept 1 c
        _            -> reject

literal :: Rule Char Token
literal = rule $ \case
    '\"':_          -> String <$> string '\"'
    '\'':_          -> String <$> string '\''
    '-':_           -> Byte <$> num
    c:_ | isDigit c -> Byte <$> num
    _               -> reject

tokenize :: Rule Char Token
tokenize = rule $ \case
    c:_ | isAlpha c                  -> op
    c:_ | elem c "\"\'" || isDigit c -> literal
    _                                -> reject

tokens :: Rule (Pos, Char) [(Pos, Token)]
tokens = separated (overM tokenize) (overM (ws <|> nl))

parse :: FilePath -> String -> Result Msg [(Pos, Token)]
parse fp = expect fp . run tokens . position fp

