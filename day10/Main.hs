module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show input)

    -- Solve
    let iterations = iterate step input
    let answer1 = length $ iterations !! 40
    let answer2 = length $ iterations !! 50

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

step :: String -> String
step [] = []
step (x:xs) = n':x:(step xs')
    where n = length $ takeWhile (==x) xs
          xs' = drop n xs
          n' = head . show $ n + 1
