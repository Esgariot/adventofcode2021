module Main where

import System.Environment
import System.Exit
import Data.Bits (complement)

data Bit = I | O deriving (Show) -- such that Bit list is single line of input

flipBit I = O
flipBit O = I

parseDiag :: [Char] -> [Bit]
parseDiag = map bit
  where
    bit '1' = I
    bit '0' = O
    bit _ = error "Unexpected bit"

-- TODO: Rename
overall :: [[Bit]] -> [Bit]
overall diags = toBit <$> foldr update (repeat 0) diags
  where
    update = zipWith upd1
    upd1 I = (+ 1)
    upd1 O = (+ (-1))
    toBit n
      | n < 0 = O
      | n > 0 = I
      | otherwise = error "unexpected count"

toNum bits@(b:bs) = ((2^(length bits - 1)) * val b) + toNum bs
        where val I = 1
              val O = 0
toNum [] = 0

gammaEpsilon:: [Bit] -> Int
gammaEpsilon bits = toNum bits * toNum (map flipBit bits) 

parse ["-h"] = usage >> exitSuccess
parse [input] = readFile input
parse _ = usage >> exitFailure

usage = putStrLn "provide path to file containing list of numbers, one per line."

main = getArgs >>= parse >>= print . gammaEpsilon . overall . map parseDiag . lines
