module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let grid = [(x,y) | x <- [0..999], y <- [0..999]] :: [(Int,Int)]

    let isOnAfterCmds (x,y) =
            1 == foldl (\acc (r1,r2,action) ->
                     if isInRect (x,y) (r1,r2)
                     then case action of
                            2 -> (1-acc)
                            _ -> action
                     else acc) 0 parsedInput
    let answer1 = length $ filter (isOnAfterCmds) grid

    let brightnessAfterCmds (x,y) =
            foldl (\acc (r1,r2,action) ->
                if isInRect (x,y) (r1,r2)
                then case action of
                       0 -> max 0 (acc-1)
                       1 -> acc+1
                       _ -> (2+acc)
                else acc) 0 parsedInput :: Int
    let answer2 = sum $ map (brightnessAfterCmds) grid

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

isInRect :: Point -> (Point, Point) -> Bool
isInRect (x,y) ((x1,y1),(x2,y2)) = 
    x >= min x1 x2 && x <= max x1 x2 && y >= min y1 y1 && y <= max y1 y2

type Point = (Int,Int)
type Instruction = (Point,Point,Int)
parse :: String -> [Instruction]
parse input = map (parseOne . words) $ lines input
    where parseOne ("turn":state:start:_:end:[]) =
                (read ("("++start++")"), read ("("++end++")"),
                 if state == "on" then 1 else 0)
          parseOne ("toggle":start:_:end:[]) = 
                (read ("("++start++")"), read ("("++end++")"), 2)
          parseOne line = error $ "Malformatted input line " ++ unwords line
