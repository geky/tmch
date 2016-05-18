module Asm where

import Prelude hiding (error)
import Data.Char
import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad
import Parse
import Result
import Pos
import Encode
import Mem


type Label = String


size :: Token -> Int
size = \case
    Label _     -> 0
    Byte _      -> 1
    String s    -> length s
    Ins op args -> force (encoded <|> pure 0)
      where
        encoded = length <$> do
            args <- mapM (asmArg (const $ ok 0)) args
            ins op args

asmArg 
    :: (Label -> Result String Int)
    -> Either String Int -> Result String Arg
asmArg l arg = case arg of
    Left r  -> case reg r of
        Error _ -> Imm <$> l r
        r       -> Reg <$> r
    Right i -> Imm <$> ok i

asmJump :: [(Label, Int)] -> Int -> Label -> Result String Int
asmJump ls from l = case lookup l ls of
    Just to -> ok (to - from)
    _       -> error ("unknown label \"" ++ l ++ "\"")

asmIns :: [(Label, Int)] -> Int -> Token -> Result String [Word8]
asmIns ls off = \case
    Label _         -> ok []
    Byte b          -> u8 b
    String s        -> concat <$> mapM (u8 . ord) s
    t@(Ins op args) -> do
        args <- mapM (asmArg (asmJump ls (off + size t))) args
        ins op args
    
asmLabel :: [(Label, Int)] -> Label -> Result String Int
asmLabel ls l = case lookup l ls of
    Just offset -> ok offset
    _           -> error ("unknown label \"" ++ l ++ "\"")

address :: [Token] -> [(Int, Token)]
address ts = zip (scanl (+) 0 (map size ts)) ts

labels :: [Token] -> [(Label, Int)]
labels ts = mapMaybe label (address ts)
  where
    label = \case
        (off, Label l) -> Just (l, off)
        _              -> Nothing
    
assemble :: [(Pos, Token)] -> Result Msg Mem8
assemble psts = mem 0 <$> (concat <$> psds)
  where
    (ps, ts) = unzip psts
    ds = map (uncurry $ asmIns (labels ts)) (address ts)
    psds = zipWithM resultMsg ps ds

