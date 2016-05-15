module Encode where

import Prelude hiding (error)
import Data.Int
import Data.Word
import Data.Bits
import Result


data Arg
    = Reg Reg
    | Imm Int
  deriving Show

data Reg = A | B | SP | PC
  deriving (Show, Enum)


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


ins :: String -> [Arg] -> Result String [Word8]
ins = curry $ \case
    ("mv",   [Reg rd, Reg ra]) -> ok [0x00 .|. dst rd .|. src ra]
    ("mvi",  [Reg rd, Reg ra]) -> ok [0x10 .|. dst rd .|. src ra]
    ("st",   [Reg rd, Reg ra]) -> ok [0x20 .|. dst rd .|. src ra]
    ("sti",  [Reg rd, Reg ra]) -> ok [0x30 .|. dst rd .|. src ra]

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

    ("mv",   [rd, Imm i]) -> concatM [ins "mvi"  [rd, Reg PC], u8 i]
    ("st",   [rd, Imm i]) -> concatM [ins "sti"  [rd, Reg PC], u8 i]
    ("cz",   [rd, Imm i]) -> concatM [ins "czi"  [rd, Reg PC], u8 i]
    ("cnz",  [rd, Imm i]) -> concatM [ins "cnzi" [rd, Reg PC], u8 i]
    ("and",  [rd, Imm i]) -> concatM [ins "andi" [rd, Reg PC], u8 i]
    ("xor",  [rd, Imm i]) -> concatM [ins "xori" [rd, Reg PC], u8 i]
    ("add",  [rd, Imm i]) -> concatM [ins "addi" [rd, Reg PC], u8 i]
    ("sub",  [rd, Imm i]) -> concatM [ins "subi" [rd, Reg PC], u8 i]

    ("ld", args) -> ins "mvi" args

    ("b",   [Imm i]) -> concatM [ins "subi" [Reg PC, Reg PC], i8 (negate i)]
    ("bz",  [Imm i]) -> concatM [ins "czi"  [Reg PC, Reg PC], i8 (negate i)]
    ("bnz", [Imm i]) -> concatM [ins "cnzi" [Reg PC, Reg PC], i8 (negate i)]

    ("nop",  []) -> ins "mv" [Reg A, Reg A]
    ("halt", []) -> ins "b" [Imm (-2)]
    ("swi", [Imm i]) -> concatM [ins "sti" [Reg PC, Reg PC], u8 i]

    (op, _) -> error ("invalid instruction \"" ++ op ++ "\"")
  where
    concatM = fmap concat . sequence

