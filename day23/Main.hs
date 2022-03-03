module Main where

import qualified Data.Map as M
import Data.List (isInfixOf,isPrefixOf)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = process 0 parsedInput $ M.fromList [("a",0),("b",0)]
    let answer2 = process 0 parsedInput $ M.fromList [("a",1),("b",0)]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Instruction = (String,Int)
parse :: String -> [Instruction]
parse input = map readOne $ lines input
    where readOne = readSnd . Common.splitOnceOn ","
          readSnd (a,b)
            | isInfixOf "+" a = readSnd $ Common.splitOnceOn " " a
            | isInfixOf "-" a = readSnd $ Common.splitOnceOn " " a
            | length b == 0 = (a,0)
            | otherwise     = (a,read $ filter (/='+') b)

process :: Int -> [Instruction] -> M.Map String Int -> Int
process pointer instructions registers
  | pointer < 0 || pointer >= length instructions = registers M.! "b"
  | otherwise = process (pointer+offset) instructions registers'
  where (offset,registers') = processOne (instructions !! pointer) registers

processOne :: Instruction -> M.Map String Int -> (Int,M.Map String Int)
processOne (operation,offset) registers
  | isPrefixOf "hlf" operation = (1, Common.mapModify r (`div` 2) registers)
  | isPrefixOf "tpl" operation = (1, Common.mapModify r (*3) registers)
  | isPrefixOf "inc" operation = (1, Common.mapModify r (+1) registers)
  | isPrefixOf "jmp" operation = (offset, registers)
  | isPrefixOf "jie" operation = if even $ registers M.! r
                                 then (offset, registers)
                                 else (1, registers)
  | isPrefixOf "jio" operation = if registers M.! r == 1
                                 then (offset, registers)
                                 else (1, registers)
  | otherwise = error "Unrecognized operation"
  where r = snd $ Common.splitOnceOn " " operation
