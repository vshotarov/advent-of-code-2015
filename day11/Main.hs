module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show input)

    -- Solve
    let answer1 = next input
    let answer2 = next answer1

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

next :: String -> String
next = Common.firstWhere isValid . iterate increment . increment
    where hasStraight [] = False
          hasStraight xs | length xs < 3 = False
          hasStraight (x:y:z:_) | z == succ y && y == succ x = True
          hasStraight (_:xs) = hasStraight xs
          hasBadChars pass = any (`elem` pass) "iol"
          containsPair xs = (>1) . length
                          . filter (uncurry (==)) -- only pairs of the same char
                          . Common.unique . zip xs $ tail xs -- unique pairs
          isValid pass = hasStraight pass
                       && (not $ hasBadChars pass)
                       && containsPair pass

increment :: String -> String
increment = reverse . go . reverse
    where go [] = error "increment: Can't increment empty string"
          go ('z':xs) = 'a':go xs
          go (x:xs) = (succ x):xs

{-
Example input:
abcdefgh

Real input:
hepxcrrq
-}
