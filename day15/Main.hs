module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day15 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    -- NOTE: Ideally i would like to use this version of getting all the possible
    --       combinations of N numbers adding up to 100, where N is the number
    --       of ingredients, but it's way slower than the list comprehension for
    --       some reason, so I am going to stick with it, even though, it's
    --       hardcoded for 4 ingredients
    --let combos = filter ((==100) . sum)
    --           . sequence $ replicate (length parsedInput) [1..100]
    let combos = [[x,y,z,w] | x <- [1..100], y <- [1..100],
                              z <- [1..100], w <- [1..100],
                              (x+y+z+w)==100] :: [[Int]]
    let recipe combo = foldr (zipWith (+)) [0,0,0,0,0]
                      . map (\(n,amounts) -> map (*n) amounts)
                      $ zip combo parsedInput
    let recipes = map recipe combos
    let scores = map (foldr1 (*) . map (max 0) . init) recipes
    let answer1 = maximum scores
    let answer2 = maximum . map snd . filter (\(r,_) -> last r == 500)
                $ zip recipes scores

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = map parseOne $ lines input
    where parseOne x = amounts
              where (_,ingredients) = Common.splitOnceOn ": " x
                    ingredients' = filter (/=',') ingredients
                    amounts = map read . Common.everyNth 2 . tail $ words ingredients'
