module Main where

import qualified Common
import qualified Data.Set as Set

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let addVec2 (x1,y1) (x2,y2) = (x1+x2,y1+y2)
    let answer1 = length . Set.fromList $ scanl addVec2 (0,0) parsedInput
    let answer2 = length . Set.fromList . Common.flattenTuples2
                $ scanl (\(s,r) d -> (r,addVec2 s d)) ((0,0),(0,0)) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [(Int,Int)]
parse input = map parseOne input
    where parseOne '^' = ( 0, 1)
          parseOne '>' = ( 1, 0)
          parseOne '<' = (-1, 0)
          parseOne 'v' = ( 0,-1)
          parseOne x   = error $ "Unrecognized direction " ++ [x]
