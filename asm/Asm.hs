module Asm where

import Prelude hiding (error)
import Data.Char
import Data.Word
import Data.Maybe
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import Parse
import Result
import Pos
import Encode
import Mem


type Label = String


asmArg 
    :: (Label -> Result String Int)
    -> Either String Int -> Result String Arg
asmArg l arg = case arg of
    Left r  -> case reg r of
        Error _ -> Imm <$> l r
        r       -> Reg <$> r
    Right i -> Imm <$> ok i

asmIns
    :: (Label -> Result String Int)
    -> Int -> Token -> Result String [Word8]
asmIns l pc = \case
    Label _     -> ok []
    Byte b      -> u8 b
    String s    -> concat <$> mapM (u8 . ord) s
    Ins op args -> do
        args <- mapM (asmArg l) args
        ins op args pc
        
asmLabel :: [(Label, Int)] -> Label -> Result String Int
asmLabel ls l = case lookup l ls of
    Just offset -> ok offset
    _           -> error ("unknown label \"" ++ l ++ "\"")


address :: Int -> [Token] -> [(Int, Token)]
address pc = \case
    (t:ts) -> (pc, t) : address (pc + length ds) ts
      where ds = force (asmIns (const $ ok 0) pc t <|> pure [])
    _      -> []

labels :: [Token] -> [(Label, Int)]
labels ts = mapMaybe label (address 0 ts)
  where
    label = \case
        (off, Label l) -> Just (l, off)
        _              -> Nothing

assemble :: [(Pos, Token)] -> Result Msg Mem8
assemble ts
    = fmap (mem 0 . concat)
    $ uncurry (zipWithM resultMsg)
    $ second (map (uncurry $ asmIns (asmLabel ls)) . address 0)
    $ unzip ts
  where
    ls = labels (map snd ts)
