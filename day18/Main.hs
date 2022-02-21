module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day18 --"
    input <- Common.readInput

    -- Parse
    let (grid,width,height) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show grid)

    -- Solve
    let corners = [(x',y') | x' <- [0,width-1], y' <- [0,height-1]]
    let neighbours (x,y) = [(x+x',y+y') | x' <- [-1..1], y' <- [-1..1],
                                          not (x' == 0 && y' == 0),
                                          x+x' >= 0 && x + x' < width,
                                          y+y' >= 0 && y + y' < height]
    let get state (x,y) = (state !! y) !! x
    let nextValue cornersStuck state (x,y)
            | cornersStuck && (x,y) `elem` corners = 1
            | otherwise = fromEnum (if get state (x,y) == 1
                                    then numN `elem` [2,3] else numN == 3)
              where numN = sum . map (get state) $ neighbours (x,y)
    let step cornersStuck state = map (\y -> map (step' y) [0..width-1]) [0..height-1]
            where step' y x = nextValue cornersStuck state (x,y)
    let answer1 = sum . map sum . last . take 101 $ iterate (step False) grid
    let answer2 = sum . map sum . last . take 101 $ iterate (step True) grid

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> ([[Int]],Int,Int)
parse input = (grid,length $ grid !! 0, length grid)
    where grid = map (map (\c -> fromEnum $ c == '#')) $ lines input
