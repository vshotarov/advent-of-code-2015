module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day14 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = maximum $ map (performRace 2503) parsedInput

    let states = map (\i -> map (performRace i) parsedInput) [1..2503]
    let winner state = map (fromEnum . (==maximum')) state
            where maximum' = maximum state
    let answer2 = maximum $ foldl
                              (zipWith (+))
                              (repeat 0) -- initial state (no wins)
                            $ map winner states

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Reindeer = (Int,Int,Int)
parse :: String -> [Reindeer]
parse input = map parseOne $ lines input
    --                             v speed         v fly period    v rest period
    where parseOne x = (read $ x' !! 3, read $ x' !! 6, read $ x' !! 13)
              where x' = words x

performRace :: Int -> Reindeer -> Int
performRace seconds (speed,flyTime,restTime) =
    totalFlySeconds * speed
        where periodTime = flyTime + restTime
              periods = seconds `div` periodTime
              remainder = seconds - periods * periodTime
              totalFlySeconds = periods * flyTime
                              + min flyTime remainder
