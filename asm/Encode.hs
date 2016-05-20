module Encode where

import Prelude hiding (error)
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative
import Result


data Arg
    = Reg Reg
    | Imm Int
  deriving Show

data Reg = A | B | SP | PC
  deriving (Show, Enum)


isImm :: Arg -> Bool
isImm = \case
    Imm _ -> True
    _     -> False

isReg :: Arg -> Bool
isReg = \case
    Reg _ -> True
    _     -> False

dst :: Reg -> Word8
dst = (`shift` 2) . fromIntegral . fromEnum

src :: Reg -> Word8
src = fromIntegral . fromEnum

i8 :: Int -> Result String [Word8]
i8 i
  | i >= min && i <= max = ok [fromIntegral i]
  | otherwise = error ("value out of range \"" ++ show i ++ "\"")
  where
    min = fromIntegral (minBound :: Int8)
    max = fromIntegral (maxBound :: Int8)

u8 :: Int -> Result String [Word8]
u8 i
  | i >= min && i <= max = ok [fromIntegral i]
  | otherwise = error ("value out of range \"" ++ show i ++ "\"")
  where
    min = fromIntegral (minBound :: Word8)
    max = fromIntegral (maxBound :: Word8)

reg :: String -> Result String Reg
reg = \case
    "r0" -> ok A
    "r1" -> ok B
    "r2" -> ok SP
    "r3" -> ok PC

    "a"  -> ok A
    "b"  -> ok B
    "sp" -> ok SP
    "pc" -> ok PC

    r -> error ("unknown register \"" ++ r ++ "\"")

ins :: Int -> String -> [Arg] -> Result String [Word8]
ins pc = curry $ \case
    ("mv",  [Reg rd, Reg ra]) -> ok [0x00 .|. dst rd .|. src ra]
    ("mvi", [Reg rd, Reg ra]) -> ok [0x10 .|. dst rd .|. src ra]
    ("ld",  [Reg rd, Reg ra]) -> ok [0x10 .|. dst rd .|. src ra]
    ("st",  [Reg rd, Reg ra]) -> ok [0x20 .|. dst rd .|. src ra]
    ("sti", [Reg rd, Reg ra]) -> ok [0x30 .|. dst rd .|. src ra]

    ("cz",   [Reg rd, Reg ra]) -> ok [0x40 .|. dst rd .|. src ra]
    ("czi",  [Reg rd, Reg ra]) -> ok [0x50 .|. dst rd .|. src ra]
    ("cnz",  [Reg rd, Reg ra]) -> ok [0x60 .|. dst rd .|. src ra]
    ("cnzi", [Reg rd, Reg ra]) -> ok [0x70 .|. dst rd .|. src ra]

    ("and",  [Reg rd, Reg ra]) -> ok [0x80 .|. dst rd .|. src ra]
    ("andi", [Reg rd, Reg ra]) -> ok [0x90 .|. dst rd .|. src ra]
    ("xor",  [Reg rd, Reg ra]) -> ok [0xa0 .|. dst rd .|. src ra]
    ("xori", [Reg rd, Reg ra]) -> ok [0xb0 .|. dst rd .|. src ra]
    ("add",  [Reg rd, Reg ra]) -> ok [0xc0 .|. dst rd .|. src ra]
    ("addi", [Reg rd, Reg ra]) -> ok [0xd0 .|. dst rd .|. src ra]
    ("sub",  [Reg rd, Reg ra]) -> ok [0xe0 .|. dst rd .|. src ra]
    ("subi", [Reg rd, Reg ra]) -> ok [0xf0 .|. dst rd .|. src ra]

    ("mv",  [rd, Imm i]) -> liftA2 (++) (ins pc "mvi"  [rd, Reg PC]) (u8 i)
    ("st",  [rd, Imm i]) -> liftA2 (++) (ins pc "sti"  [rd, Reg PC]) (u8 i)
    ("cz",  [rd, Imm i]) -> liftA2 (++) (ins pc "czi"  [rd, Reg PC]) (i8 i)
    ("cnz", [rd, Imm i]) -> liftA2 (++) (ins pc "cnzi" [rd, Reg PC]) (i8 i)
    ("and", [rd, Imm i]) -> liftA2 (++) (ins pc "andi" [rd, Reg PC]) (u8 i)
    ("xor", [rd, Imm i]) -> liftA2 (++) (ins pc "xori" [rd, Reg PC]) (u8 i)
    ("add", [rd, Imm i]) -> liftA2 (++) (ins pc "addi" [rd, Reg PC]) (i8 i)
    ("sub", [rd, Imm i]) -> liftA2 (++) (ins pc "subi" [rd, Reg PC]) (i8 i)

    ("mv",  rd:is) | all isImm is -> inss pc $ map (\i -> ("mv",  [rd, i])) is
    ("mv",  rd:is) | all isImm is -> inss pc $ map (\i -> ("mv",  [rd, i])) is
    ("st",  rd:is) | all isImm is -> inss pc $ map (\i -> ("st",  [rd, i])) is
    ("mvi", rd:rs) | all isReg rs -> inss pc $ map (\r -> ("mvi", [rd, r])) rs
    ("ld",  rd:rs) | all isReg rs -> inss pc $ map (\r -> ("ld",  [rd, r])) rs
    ("st",  rd:rs) | all isReg rs -> inss pc $ map (\r -> ("st",  [rd, r])) rs
    ("sti", rd:rs) | all isReg rs -> inss pc $ map (\r -> ("sti", [rd, r])) rs

    ("push", is) | all isImm is -> inss pc $ map (\i -> ("st", [Reg SP, i])) is
    ("push", rs) | all isReg rs -> inss pc $ map (\r -> ("st", [Reg SP, r])) rs
    ("pop",  rs) | all isReg rs -> inss pc $ map (\r -> ("ld", [r, Reg SP])) rs
    ("pop",  rs) | all isReg rs -> inss pc $ map (\r -> ("ld", [r, Reg SP])) rs

    ("b",   [Imm i]) -> ins pc "sub" [Reg PC, Imm (negate (i-(pc+2)))]
    ("bz",  [Imm i]) -> ins pc "cz"  [Reg PC, Imm (negate (i-(pc+2)))]
    ("bnz", [Imm i]) -> ins pc "cnz" [Reg PC, Imm (negate (i-(pc+2)))]

    ("call", [Reg A]) -> inss pc
        [ ("mv", [Reg B, Reg PC])
        , ("mv", [Reg PC, Reg A])
        ]
    ("call", [Reg r]) -> inss pc
        [ ("mv", [Reg A, Reg r])
        , ("call", [Reg A])
        ]
    ("call", is) | all isImm is -> inss pc
        [ ("mv", Reg A:is)
        , ("call", [Reg A])
        ]
    ("ret", []) -> inss pc
        [ ("add", [Reg B, Imm 1])
        , ("mv", [Reg PC, Reg B])
        ]

    ("nop",  []) -> ins pc "mv" [Reg A, Reg A]
    ("halt", []) -> ins pc "b" [Imm (-2)]
    ("swi",  [Imm i]) -> ins pc "st" [Reg PC, Imm i]

    (op, args) | not (null op) && isDigit (last op) && not (null args) ->
        case last args of
            Imm i -> ins pc op' (init args ++ shifts n' i)
            Reg r -> ins pc op' (init args ++ replicate n' (Reg r))
          where 
            (op', n') = (init op, digitToInt (last op))
            shifts n = map Imm 
                . (\(s:ss) -> s : map (0xff .&.) ss)
                . reverse . take n . iterate (`shift` (-8))

    (op, _) -> error ("invalid instruction \"" ++ op ++ "\"")

inss :: Int -> [(String, [Arg])] -> Result String [Word8]
inss pc = \case
    (op, arg):is -> liftA2 (++) i (inss (pc + length i) is)
      where i = ins pc op arg
    _            -> ok []



