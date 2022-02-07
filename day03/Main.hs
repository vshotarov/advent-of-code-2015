module Main where

import qualified Data.Set as Set
import qualified Common
import Vec (Vec2(..))

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length . Set.fromList $ scanl (+) (Vec2 0 0) parsedInput
    let answer2 = length . Set.fromList . Common.flattenTuples2
                $ scanl (\(s,r) d -> (r,s + d)) ((Vec2 0 0),(Vec2 0 0)) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Vec2 Int]
parse input = map parseOne input
    where parseOne '^' = Vec2   0   1
          parseOne '>' = Vec2   1   0
          parseOne '<' = Vec2 (-1)  0
          parseOne 'v' = Vec2   0 (-1)
          parseOne x   = error $ "Unrecognized direction " ++ [x]
