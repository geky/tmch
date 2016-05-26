module Asm where

import Prelude hiding (error)
import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad
import Parse
import Result
import Pos
import Encode
import Mem


asmImm :: (Label -> Result String Int) -> TokenArg -> Result String Int
asmImm l arg = asmArg l arg >>= \case
    Imm i -> ok i
    Reg r -> error ("invalid use of register \"" ++ show r ++ "\"")

asmReg :: (Label -> Result String Int) -> TokenArg -> Result String Reg
asmReg l arg = asmArg l arg >>= \case
    Reg r -> ok r
    Imm i -> error ("invalid use of immediate \"" ++ show i ++ "\"")

asmArg :: (Label -> Result String Int) -> TokenArg -> Result String Arg
asmArg l arg = case arg of
    Symbol r -> case reg r of
        Error _ -> Imm <$> l r
        r       -> Reg <$> r
    Literal a -> Imm <$> ok a
    Add a b   -> Imm <$> liftA2 (+) (asmImm l a) (asmImm l b)
    Sub a b   -> Imm <$> liftA2 (-) (asmImm l a) (asmImm l b)

asmIns :: (Label -> Result String Int) -> Int -> Token -> Result String [Word8]
asmIns l pc = \case
    Pseudo "byte" is -> concat <$> mapM (u8 <=< asmImm l) is
    Pseudo op _ -> error ("unknown pseudo-op \"" ++ op ++ "\"")
    Label _     -> ok []
    Ins op args -> do
        args <- mapM (asmArg l) args
        ins op args pc

asmLabel :: [(Label, Int)] -> Label -> Result String Int
asmLabel ls l = case lookup l ls of
    Just offset -> ok offset
    _           -> error ("unknown label \"" ++ l ++ "\"")


address :: Int -> [(Pos, Token)] -> Result Msg [(Int, (Pos, Token))]
address pc = \case
    ((p, Pseudo "org" [org]):ts) -> do
        pc <- resultMsg p $ asmImm lerror org
        address pc ts
    ((p, t):ts) -> do
        ds <- resultMsg p
            $   asmIns (\_ -> ok 0) pc t
            <|> asmIns (\_ -> ok pc) pc t
            <|> pure []
        ((pc, (p, t)):) <$> address (pc + length ds) ts
    _ -> okMsg []
  where lerror l = error ("invalid use of label \"" ++ l ++ "\"")

labels :: [(Pos, Token)] -> [(Label, Int)]
labels ts = mapMaybe label (force (address 0 ts <|> pure []))
  where
    label = \case
        (off, (_, Label l)) -> Just (l, off)
        _                   -> Nothing

assemble :: [(Pos, Token)] -> Result Msg Mem8
assemble ts = do
    let ls = labels ts
    as <- address 0 ts
    es <- mapM (asm ls) as
    return $ foldl merge zeros es
  where
    asm ls (pc, (p, t)) = resultMsg p $ do
        ds <- asmIns (asmLabel ls) pc t
        return $ mem pc ds
