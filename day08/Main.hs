module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = foldl (\acc x -> acc + length x - numMemChars x) 0 parsedInput
    let answer2 = foldl (\acc x -> acc + numEncodedChars x - length x) 0 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

numMemChars :: String -> Int
numMemChars x = length $ (read $ clean x :: String)
    where clean [] = []
          clean ('\\':'x':_:_:xs) = 'C':clean xs
          clean ('\\':'\\':xs) = 'C':clean xs
          clean (x':xs) = x':clean xs

numEncodedChars :: String -> Int
numEncodedChars x = length $ show x
