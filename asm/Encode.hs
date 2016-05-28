module Encode where

import Prelude hiding (error)
import Data.Word
import Data.Bits
import Control.Applicative
import Control.Monad
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

bytes :: Int -> Int -> Result e [Word8]
bytes n = ok
    . map fromIntegral
    . reverse 
    . take n
    . iterate (`shift` (-8))

word :: Int -> Result String [Word8]
word i
    | i >= -0x8000 && i <= 0xffff = bytes 2 i
    | otherwise = error ("value out of range \"" ++ show i ++ "\"")

byte :: Int -> Result String [Word8]
byte i
    | i >= -0x80 && i <= 0xff = bytes 1 i
    | otherwise = error ("value out of range \"" ++ show i ++ "\"")

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

    ("mv",  [rd, Imm i]) -> comp pc [ins "mvi"  [rd, Reg PC], const (byte i)]
    ("st",  [rd, Imm i]) -> comp pc [ins "sti"  [rd, Reg PC], const (byte i)]
    ("cz",  [rd, Imm i]) -> comp pc [ins "czi"  [rd, Reg PC], const (byte i)]
    ("cnz", [rd, Imm i]) -> comp pc [ins "cnzi" [rd, Reg PC], const (byte i)]
    ("and", [rd, Imm i]) -> comp pc [ins "andi" [rd, Reg PC], const (byte i)]
    ("xor", [rd, Imm i]) -> comp pc [ins "xori" [rd, Reg PC], const (byte i)]
    ("add", [rd, Imm i]) -> comp pc [ins "addi" [rd, Reg PC], const (byte i)]
    ("sub", [rd, Imm i]) -> comp pc [ins "subi" [rd, Reg PC], const (byte i)]

    ("mvw",  [rd, Imm i]) -> wordi pc id      (\i -> ins "mv"  [rd, i]) i
    ("stw",  [rd, Imm i]) -> wordi pc reverse (\i -> ins "st"  [rd, i]) i
    ("mviw", [rd, Reg r]) -> wordr pc id      (\r -> ins "mvi" [rd, r]) r
    ("ldw",  [rd, Reg r]) -> wordr pc id      (\r -> ins "ld"  [rd, r]) r
    ("stw",  [rd, Reg r]) -> wordr pc id      (\r -> ins "st"  [rd, r]) r

    ("push",  [r]) -> ins "st"  [Reg SP, r] pc
    ("pop",   [r]) -> ins "ld"  [r, Reg SP] pc
    ("pushw", [r]) -> ins "stw" [Reg SP, r] pc
    ("popw",  [r]) -> ins "ldw" [r, Reg SP] pc

    ("b",   [Imm i]) -> branch pc (\i -> ins "sub" [Reg PC, Imm i]) i
    ("bz",  [Imm i]) -> branch pc (\i -> ins "cz"  [Reg PC, Imm i]) i
    ("bnz", [Imm i]) -> branch pc (\i -> ins "cnz" [Reg PC, Imm i]) i

    ("call", [Reg A]) -> comp pc
        [ ins "mv" [Reg B, Reg PC]
        , ins "mv" [Reg PC, Reg A]
        ]
    ("call", [Reg r]) -> comp pc
        [ ins "mv" [Reg A, Reg r]
        , ins "call" [Reg A]
        ]
    ("call", [Imm i]) -> comp pc
        [ ins "mvw" [Reg A, Imm i]
        , ins "call" [Reg A]
        ]
    ("ret", []) -> comp pc
        [ ins "add" [Reg B, Imm 1]
        , ins "mv" [Reg PC, Reg B]
        ]

    ("nop",  []) -> ins "mv" [Reg A, Reg A] pc
    ("halt", []) -> ins "sub" [Reg PC, Imm 2] pc
    ("swi",  [Imm i]) -> ins "st" [Reg PC, Imm i] pc

    (op, _) -> error ("invalid instruction \"" ++ op ++ "\"")
  where
    comp pc = \case
        f:fs -> liftA2 (++) (f pc) (comp (pc + length (f pc)) fs)
        _    -> pure []

    wordi pc r f = comp pc . r . map (f . Imm . fromIntegral) <=< word
    wordr pc r f = comp pc . r . map (f . Reg) . replicate 2

    branch pc f b
        | i >= -0x80 && i <= 0x7f = f i pc
        | otherwise = error ("branch out of range \"" ++ show (-i) ++ "\"")
        where i = negate (b-(pc+2))

