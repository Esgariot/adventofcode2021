module Main where
import System.Environment
import System.Exit

count (x1:x2:xs)
        | x1 < x2 = 1 + count (x2:xs)
        | otherwise =  count (x2:xs)
count _ = 0

rollingSum3 (x1:x2:x3:xs) = (x1+x2+x3):rollingSum3 (x2:x3:xs)
rollingSum3 _ = []

convert = count . rollingSum3 . map (\l -> read l :: Int) . lines

parse ["-h"] = usage >> exitSuccess 
parse [input] = readFile input
parse _ = usage >> exitFailure 

usage = putStrLn "provide path to file containing list of numbers, one per line."

main = getArgs >>= parse >>=  print . convert 
