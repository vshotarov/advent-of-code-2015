module Main where

import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.Bits ((.&.),(.|.),complement,shiftL,shiftR)
import qualified Data.Map as M
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let initialValues = M.map (read . head)
                      $ M.filter
                        (\op -> length op == 1 && (all isDigit $ head op))
                        parsedInput
    let answer1 = fst $ get parsedInput initialValues "a"
    let answer2 = fst $ get parsedInput (M.insert "b" answer1 initialValues) "a"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Operation = [String]
type Instructions = M.Map String Operation
type Values = M.Map String Int

parse :: String -> Instructions
parse input = M.fromList . map parseLine $ lines input
    where parseLine line = case splitOn "->" line of
                             [left,right] -> (head $ words right, words left)
                             _ -> error $ "Malformatted input line " ++ line

get :: Instructions -> Values -> String -> (Int,Values)
get _ values w | all isDigit w = (read w, values)
get _ values w | M.member w values = (values M.! w, values)
get instructions values w = perform instructions values w

perform :: Instructions -> Values -> String -> (Int,Values)
perform instructions values w =
    case instructions M.! w of
      [x,op,y] -> (v, M.insert w v values'')
          where (x',values') = get instructions values x
                (y',values'') = get instructions values' y
                v = case op of
                      "AND"     -> x' .&. y'
                      "OR"      -> x' .|. y'
                      "LSHIFT"  -> shiftL x' y'
                      "RSHIFT"  -> shiftR x' y'
                      _         -> error $ "Unrecognized op - " ++ op
      ["NOT",x] -> ((twoTo16 + v) `mod` twoTo16, M.insert w v values')
          where (x',values') = get instructions values x
                v = complement x'
                twoTo16 = 2^(16::Int)
      [x] -> get instructions values x
      instruction -> error $ "Unrecognized instruction - " ++ show instruction
