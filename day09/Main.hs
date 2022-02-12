module Main where

import qualified Data.Map as M
import Data.List (permutations)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let (graph,locations) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show graph)

    -- Solve
    let pathLength path = sum . map (graph M.!) $ zip path $ tail path
    let paths = Common.sortBySnd . map (\p -> (p,pathLength p))
              $ permutations locations
    let answer1 = head paths
    let answer2 = last paths

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> (M.Map (String,String) Int,[String])
parse input = (graph, Common.unique $ map (fst . fst) parsed)
    where parsed = concat . map parseOne $ lines input
          graph = M.fromList parsed
          parseOne x = let (left,right) = Common.splitOnceOn " = " x
                           (from,to) = Common.splitOnceOn " to " left
                           distance = read right :: Int
                        in [((from,to),distance),((to,from),distance)]
