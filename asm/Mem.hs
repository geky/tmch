module Mem where

import Data.Array.Unboxed hiding ((!), (//), assocs)
import qualified Data.Array.IArray as Array

import Control.Arrow
import Data.Char
import Data.List
import Data.Word
import Data.Bits


-- Mem type --
data Mem a
    = IndirectMem Int (Array Int (Mem a)) 
    | DirectMem (UArray Int a)
    | NoMem
type Dat d = (Integral d, IArray UArray d)

type Mem8 = Mem Word8
type Mem16 = Mem Word16
type Mem32 = Mem Word32
type Mem64 = Mem Word64


mem :: Dat d => Int -> [d] -> Mem d
mem off dat = NoMem // zip [off..] dat

zeros :: Mem d
zeros = NoMem

hi :: Int -> Int -> Int
hi n i = shift i (-n)

lo :: Int -> Int -> Int
lo n i = i - shift (hi n i) n

(!) :: Dat d => Mem d -> Int -> d
(!) m i = case m of
    IndirectMem n a | inmem n i -> (a Array.! hi n i) ! lo n i
    DirectMem a     | inmem 0 i -> a Array.! i
    _                           -> 0
  where inmem n i = hi n i < 0x100

(//) :: Dat d => Mem d -> [(Int, d)] -> Mem d
(//) m us = case m of
    IndirectMem n a | all (inmem n . fst) us -> IndirectMem n (a Array.// us')
      where
        us' = map (\(i,us) -> (i, (a Array.! i) // map (first (lo n)) us))
            $ filter (not . null . snd)
            $ map (\i -> (i, filter ((==i) . hi n . fst) us)) [0..0xff]
    DirectMem a     | all (inmem 0 . fst) us -> DirectMem (a Array.// us)
    NoMem           | null us                -> NoMem

    IndirectMem n _ -> IndirectMem (n+8) (block NoMem m) // us
    DirectMem _     -> IndirectMem 8     (block NoMem m) // us
    NoMem           -> DirectMem         (block 0 0)     // us
  where
    inmem n i = hi n i < 0x100

    block :: (IArray a e) => e -> e -> a Int e
    block z e = listArray (0,0xff) (repeat z) Array.// [(0, e)]

assocs :: Dat d => Mem d -> [(Int, d)]
assocs = \case
    IndirectMem n a -> concat
        $ map (uncurry (\i -> map (first (.|. (shift i n)))))
        $ map (second assocs)
        $ Array.assocs a
    DirectMem a     -> filter ((/= 0) . snd) $ Array.assocs a
    _               -> []

merge :: Dat d => Mem d -> Mem d -> Mem d
merge a b = a // assocs b


-- Hex format --
fromHexString :: Dat d => String -> d
fromHexString = foldl (\a b -> a*16 + b) 0
    . map (fromIntegral . digitToInt)

fromHexLine :: Dat d => String -> [(Int, d)]
fromHexLine hex = zip [offset..] bytes
  where
    offset
        = fromHexString
        $ takeWhile isHexDigit
        $ hex

    bytes 
        = map fromHexString
        $ ns 2
        $ takeWhile isHexDigit 
        $ filter (not . isSpace)
        $ drop 1 . dropWhile isHexDigit
        $ hex

    ns n = \case
        [] -> []
        s  -> take n s : ns n (drop n s)

fromHex :: Dat d => String -> Mem d
fromHex = foldl (//) zeros . map fromHexLine . lines

toHexString :: Dat d => Int -> d -> String
toHexString n = reverse . take n
    . map (intToDigit . fromIntegral)
    . map (`mod` 16) . iterate (`div` 16)

toHexLine :: Dat d => Int -> [d] -> String
toHexLine offset bytes
    =  toHexString 8 offset ++ ": " 
    ++ intercalate " " (map (toHexString 2) bytes)

toHex :: Dat d => Mem d -> String
toHex a = unlines $ zipWith toHexLine offsets (map line offsets)
  where
    line offset = map (a !) [offset..offset+15]
    offsets = case map fst (assocs a) of
        [] -> []
        as -> [lower,lower+16..upper]
          where
            lower = 16 * (head as `div` 16)
            upper = last as

