module Asm where

import Prelude hiding (error)
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
    Pseudo "byte" [Right i] -> u8 i
    Pseudo op _ -> error ("unknown pseudo op \"" ++ op ++ "\"")
    Label _     -> ok []
    Ins op args -> do
        args <- mapM (asmArg l) args
        ins op args pc
        
asmLabel :: [(Label, Int)] -> Label -> Result String Int
asmLabel ls l = case lookup l ls of
    Just offset -> ok offset
    _           -> error ("unknown label \"" ++ l ++ "\"")

address :: Int -> [Token] -> [(Int, Token)]
address pc = \case
    (Pseudo "org" [Right org]:ts) -> address org ts
    (t:ts) -> (pc, t) : address (pc + length ds) ts
      where ds = force (asmIns (const (ok pc)) pc t <|> pure [])
    _  -> []

labels :: [Token] -> [(Label, Int)]
labels ts = mapMaybe label (address 0 ts)
  where
    label = \case
        (off, Label l) -> Just (l, off)
        _              -> Nothing

assemble :: [(Pos, Token)] -> Result Msg Mem8
assemble ts
    = fmap (foldl merge zeros)
    $ uncurry (zipWithM resultMsg)
    $ second (map (\(pc, t) -> mem pc <$> asmIns (asmLabel ls) pc t))
    $ second (address 0)
    $ unzip ts
  where
    ls = labels (map snd ts)
