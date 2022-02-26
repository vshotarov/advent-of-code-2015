module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day20 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = read input :: Int
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    -- Unfortunately I couldn't figure out a solution that didn't result in
    -- a stack overflow in Haskell, so I had to look it up and rip TheMuffinMan616 off
    --  https://www.reddit.com/r/adventofcode/comments/3xjpp2/comment/cy5ou98/?utm_source=share&utm_medium=web2x&context=3
    let answer1 = Common.firstIdWhere (>= parsedInput)
                $ map ((10*) . sum . factors) [0..]
    let answer2 = Common.firstIdWhere (>= parsedInput)
                $ map (\i -> (11*) . sum . filter (> (i-1) `div` 50)
                           $ factors i) [0..]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = concat $ [[x, q] | x <- [1..isqrt(n)], let (q, r) = divMod n x, r == 0]
