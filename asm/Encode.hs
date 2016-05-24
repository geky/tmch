module Encode where

import Prelude hiding (error)
import Data.Int
import Data.Word
import Data.Bits
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

ins :: String -> [Arg] -> Int -> Result String [Word8]
ins op args pc = case (op, args) of
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

    ("mv",  [rd, Imm i]) -> comp pc [ins "mvi"  [rd, Reg PC], const (u8 i)]
    ("st",  [rd, Imm i]) -> comp pc [ins "sti"  [rd, Reg PC], const (u8 i)]
    ("cz",  [rd, Imm i]) -> comp pc [ins "czi"  [rd, Reg PC], const (i8 i)]
    ("cnz", [rd, Imm i]) -> comp pc [ins "cnzi" [rd, Reg PC], const (i8 i)]
    ("and", [rd, Imm i]) -> comp pc [ins "andi" [rd, Reg PC], const (u8 i)]
    ("xor", [rd, Imm i]) -> comp pc [ins "xori" [rd, Reg PC], const (u8 i)]
    ("add", [rd, Imm i]) -> comp pc [ins "addi" [rd, Reg PC], const (i8 i)]
    ("sub", [rd, Imm i]) -> comp pc [ins "subi" [rd, Reg PC], const (i8 i)]

    ("mvw",  [rd, Imm i]) -> word pc (\i -> ins "mv"  [rd, i]) (Imm i)
    ("stw",  [rd, Imm i]) -> word pc (\i -> ins "st"  [rd, i]) (Imm i)
    ("mviw", [rd, Reg r]) -> word pc (\r -> ins "mvi" [rd, r]) (Reg r)
    ("ldw",  [rd, Reg r]) -> word pc (\r -> ins "ld"  [rd, r]) (Reg r)
    ("stw",  [rd, Reg r]) -> word pc (\r -> ins "st"  [rd, r]) (Reg r)
    ("stiw", [rd, Reg r]) -> word pc (\r -> ins "sti" [rd, r]) (Reg r)

    ("push",  [r]) -> ins "st"  [Reg SP, r] pc
    ("pop",   [r]) -> ins "ld"  [r, Reg SP] pc
    ("pushw", [r]) -> ins "stw" [Reg SP, r] pc
    ("popw",  [r]) -> ins "ldw" [r, Reg SP] pc

    ("b",   [Imm i]) -> ins "sub" [Reg PC, Imm (negate (i-(pc+2)))] pc
    ("bz",  [Imm i]) -> ins "cz"  [Reg PC, Imm (negate (i-(pc+2)))] pc
    ("bnz", [Imm i]) -> ins "cnz" [Reg PC, Imm (negate (i-(pc+2)))] pc

    ("call", [Reg A]) -> comp pc
        [ ins "mv" [Reg B, Reg PC]
        , ins "mv" [Reg PC, Reg A]
        ]
    ("call", [r]) -> comp pc
        [ ins "mv" [Reg A, r]
        , ins "call" [Reg A]
        ]
    ("callw", [r]) -> comp pc
        [ ins "mvw" [Reg A, r]
        , ins "call" [Reg A]
        ]
    ("ret", []) -> comp pc
        [ ins "add" [Reg B, Imm 1]
        , ins "mv" [Reg PC, Reg B]
        ]

    ("nop",  []) -> ins "mv" [Reg A, Reg A] pc
    ("halt", []) -> ins "b" [Imm (-2)] pc
    ("swi",  [Imm i]) -> ins "st" [Reg PC, Imm i] pc

    (op, _) -> error ("invalid instruction \"" ++ op ++ "\"")
  where
    comp pc = \case
        f:fs -> liftA2 (++) (f pc) (comp (pc + length (f pc)) fs)
        _    -> pure []

    word pc f = comp pc . map f . \case
        Imm i -> shifts 2 i 
        Reg r -> replicate 2 (Reg r)

    shifts n = map Imm 
        . (\(s:ss) -> s : map (0xff .&.) ss)
        . reverse . take n . iterate (`shift` (-8))

