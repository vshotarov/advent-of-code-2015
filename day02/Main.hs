module Main where

import Data.List.Split (splitOn)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let surfaceArea [l,w,h] = 2*l*w + 2*w*h + 2*h*l + minimum [l*w,w*h,h*l]
        surfaceArea present = error $ "Malformatted present:" ++ show present
    let ribbonLength [l,w,h] = minimum [2*(l+w),2*(w+h),2*(h+l)] + w*h*l
        ribbonLength present = error $ "Malformatted present:" ++ show present

    let answer1 = sum $ map surfaceArea parsedInput
    let answer2 = sum $ map ribbonLength parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = map parseOne $ lines input :: [[Int]]
    where parseOne = map read . splitOn "x"
