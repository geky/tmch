module Encode where

import Prelude hiding (error)
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
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
ins op args = case (prefix op, suffix op, args) of
    ("mv",  1, [Reg rd, Reg ra]) -> ok [0x00 .|. dst rd .|. src ra]
    ("mvi", 1, [Reg rd, Reg ra]) -> ok [0x10 .|. dst rd .|. src ra]
    ("ld",  1, [Reg rd, Reg ra]) -> ok [0x10 .|. dst rd .|. src ra]
    ("st",  1, [Reg rd, Reg ra]) -> ok [0x20 .|. dst rd .|. src ra]
    ("sti", 1, [Reg rd, Reg ra]) -> ok [0x30 .|. dst rd .|. src ra]

    ("cz",   1, [Reg rd, Reg ra]) -> ok [0x40 .|. dst rd .|. src ra]
    ("czi",  1, [Reg rd, Reg ra]) -> ok [0x50 .|. dst rd .|. src ra]
    ("cnz",  1, [Reg rd, Reg ra]) -> ok [0x60 .|. dst rd .|. src ra]
    ("cnzi", 1, [Reg rd, Reg ra]) -> ok [0x70 .|. dst rd .|. src ra]

    ("and",  1, [Reg rd, Reg ra]) -> ok [0x80 .|. dst rd .|. src ra]
    ("andi", 1, [Reg rd, Reg ra]) -> ok [0x90 .|. dst rd .|. src ra]
    ("xor",  1, [Reg rd, Reg ra]) -> ok [0xa0 .|. dst rd .|. src ra]
    ("xori", 1, [Reg rd, Reg ra]) -> ok [0xb0 .|. dst rd .|. src ra]
    ("add",  1, [Reg rd, Reg ra]) -> ok [0xc0 .|. dst rd .|. src ra]
    ("addi", 1, [Reg rd, Reg ra]) -> ok [0xd0 .|. dst rd .|. src ra]
    ("sub",  1, [Reg rd, Reg ra]) -> ok [0xe0 .|. dst rd .|. src ra]
    ("subi", 1, [Reg rd, Reg ra]) -> ok [0xf0 .|. dst rd .|. src ra]

    ("mv",  1, [rd, Imm i]) -> concatM [ins "mvi"  [rd, Reg PC], u8 i]
    ("st",  1, [rd, Imm i]) -> concatM [ins "sti"  [rd, Reg PC], u8 i]
    ("cz",  1, [rd, Imm i]) -> concatM [ins "czi"  [rd, Reg PC], i8 i]
    ("cnz", 1, [rd, Imm i]) -> concatM [ins "cnzi" [rd, Reg PC], i8 i]
    ("and", 1, [rd, Imm i]) -> concatM [ins "andi" [rd, Reg PC], u8 i]
    ("xor", 1, [rd, Imm i]) -> concatM [ins "xori" [rd, Reg PC], u8 i]
    ("add", 1, [rd, Imm i]) -> concatM [ins "addi" [rd, Reg PC], i8 i]
    ("sub", 1, [rd, Imm i]) -> concatM [ins "subi" [rd, Reg PC], i8 i]

    ("mv",  n, [rd, Imm i])  -> rep n "mv"  [rd, Imm i]
    ("st",  n, [rd, Imm i])  -> rep n "st"  [rd, Imm i]
    ("mvi", n, [rd, Reg ra]) -> rep n "mvi" [rd, Reg ra]
    ("ld",  n, [rd, Reg ra]) -> rep n "ld"  [rd, Reg ra]
    ("st",  n, [rd, Reg ra]) -> rep n "st"  [rd, Reg ra]
    ("sti", n, [rd, Reg ra]) -> rep n "sti" [rd, Reg ra]

    ("push", n, [Imm i])  -> rep n "st" ([Reg SP, Imm i])
    ("push", n, [Reg rd]) -> rep n "st" ([Reg SP, Reg rd])
    ("pop",  n, [Reg rd]) -> rep n "ld" ([Reg rd, Reg SP])

    ("b",   1, [Imm i]) -> ins "sub" [Reg PC, Imm (negate i)]
    ("bz",  1, [Imm i]) -> ins "cz"  [Reg PC, Imm (negate i)]
    ("bnz", 1, [Imm i]) -> ins "cnz" [Reg PC, Imm (negate i)]

    ("call", 1, [Reg A]) -> concatM 
        [ ins "mv" [Reg B, Reg PC]
        , ins "mv" [Reg PC, Reg A]
        ]
    ("call", 1, [Reg r]) -> concatM 
        [ ins "mv" [Reg A, Reg r]
        , ins "call" [Reg A]
        ]
    ("call", n, [Imm i]) -> concatM
        [ rep n "mv" [Reg A, Imm i]
        , ins "call" [Reg A]
        ]
    ("ret", 1, []) -> concatM
        [ ins "add" [Reg B, Imm 1]
        , ins "mv" [Reg PC, Reg B]
        ]

    ("nop",  1, []) -> ins "mv" [Reg A, Reg A]
    ("halt", 1, []) -> ins "b" [Imm (-2)]
    ("swi",  1, [Imm i]) -> concatM [ins "sti" [Reg PC, Reg PC], u8 i]

    (_, _, _) -> error ("invalid instruction \"" ++ op ++ "\"")
  where
    concatM = fmap concat . sequence
    
    prefix s
        | length s > 0 && isDigit (last s) = init s
        | otherwise = s

    suffix s
        | length s > 0 && isDigit (last s) = digitToInt (last s)
        | otherwise = 1

    rep n op args = case last args of
        Imm i -> concatM
            $ map (\i -> ins op (init args ++ [Imm i]))
            $ (\s -> head s : map (0xff .&.) (tail s))
            $ reverse
            $ take n
            $ iterate (`shift` (-8)) i
        Reg _ -> concatM
            $ replicate n (ins op args)
