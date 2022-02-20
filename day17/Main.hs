module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day17 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solutions = solve 150 parsedInput
    let answer1 = length solutions
    let lengthMinSolution = minimum $ map length solutions
    let answer2 = length $ filter ((==lengthMinSolution) . length) solutions

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Int]
parse input = map read $ lines input

solve :: Int -> [Int] -> [[Int]]
solve _ [] = []
solve n _ | n <= 0 = []
solve n (x:xs) = a ++ b ++ c
    where a = map (:[]) $ filter (==n) (x:xs)
          xs' = filter (/=n) xs
          b = map (x:) $ solve (n-x) xs'
          c = solve n xs'
