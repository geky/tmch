module Mem where

import Data.Ix
import Data.Array.Unboxed hiding ((!), (//), assocs)
import qualified Data.Array.Unboxed as Array
import Data.Char
import Data.List
import Data.Word


-- Mem type --
data Mem a = Mem (UArray Int a) | NoMem
type Dat d = (Integral d, IArray UArray d)

type Mem8 = Mem Word8
type Mem16 = Mem Word16
type Mem32 = Mem Word32
type Mem64 = Mem Word64


mem :: Dat d => Int -> [d] -> Mem d
mem offset dat = Mem $ listArray (offset, offset+length dat-1) dat

zeros :: Dat d => Mem d
zeros = NoMem

(!) :: Dat d => Mem d -> Int -> d
(!) m i = case m of 
    Mem a | inRange (bounds a) i -> a Array.! i
    _                            -> 0

(//) :: Dat d => Mem d -> [(Int, d)] -> Mem d
(//) m = \case
    [] -> m
    us -> Mem $ case m of
        Mem a | all (inRange (bounds a)) os -> a Array.// us
        Mem a -> listArray (lower, upper) (repeat 0)
            Array.// Array.assocs a Array.// us
          where
            lower = minimum (fst (bounds a) : os)
            upper = maximum (snd (bounds a) : os)
        _ -> listArray (lower, upper) (repeat 0)
            Array.// us
          where
            lower = minimum os
            upper = maximum os
      where
        os = map fst us

assocs :: Dat d => Mem d -> [(Int, d)]
assocs = \case
    Mem a -> filter (\(_,d) -> d /= 0) $ Array.assocs a
    _     -> []

chunk :: Dat d => Mem d -> Int -> Int -> [d]
chunk m off end = take (end - off) $ case m of
    Mem a -> take (lower - off) (repeat 0)
        ++ drop (off - lower) (elems a)
        ++ repeat 0
      where lower = fst (bounds a)
    _     -> repeat 0

merge :: Dat d => Mem d -> Mem d -> Mem d
merge a b = a // assocs b


-- Representation --
ns :: Int -> [a] -> [[a]]
ns _ [] = []
ns n s  = take n s : ns n (drop n s)

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
    line offset = chunk a offset (offset+16)
    offsets = case map fst (assocs a) of
        [] -> []
        as -> [lower,lower+16..upper]
          where
            lower = 16 * (head as `div` 16)
            upper = last as

