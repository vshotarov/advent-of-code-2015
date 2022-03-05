module Main where

import Data.List (sort)
import qualified Data.Set as S
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day24 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let total = sum parsedInput
    let groupWeight1 = total `div` 3
    let answer1 = head . sort . map product
                . S.toList
                . S.filter (doesContainSubsetWithSum groupWeight1 . S.difference parsedInput)
                . Common.firstWhere (not . S.null)
                $ map (\i -> groupsOfSizeWithSum i groupWeight1 parsedInput) [2..]
    let groupWeight2 = total `div` 4
    let answer2 = head . sort . map product
                . S.toList
                . S.filter (doesContainSubsetWithSum groupWeight2 . S.difference parsedInput)
                . Common.firstWhere (not . S.null)
                $ map (\i -> groupsOfSizeWithSum i groupWeight2 parsedInput) [2..]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type IntSet = S.Set Int
parse :: String -> IntSet
parse input = S.fromList . map read $ lines input

groupsOfSizeWithSum :: Int -> Int -> IntSet -> S.Set (IntSet)
groupsOfSizeWithSum 0 _ _ = S.empty
groupsOfSizeWithSum _ _ ns | S.null ns = S.empty
groupsOfSizeWithSum m sum' ns
  | sum' <= 0 = S.empty
  | sum' `elem` ns = S.insert
                       (S.singleton sum')
                       (groupsOfSizeWithSum m sum' $ S.filter (/=sum') ns)
  | otherwise = foldr1 (S.union)
            $ S.map (\n -> S.map (S.insert n)
                         . groupsOfSizeWithSum (m-1) (sum'-n)
                         $ S.filter (/=n) ns) ns

doesContainSubsetWithSum :: Int -> IntSet -> Bool
doesContainSubsetWithSum 0 _ = False
doesContainSubsetWithSum sum' set
  | sum' <= 0 = False
  | S.null set = False
  | sum' > sum set = False
  | sum' `S.member` set = True
  | otherwise = any (\v -> doesContainSubsetWithSum (sum'-v) $ S.filter (/=v) set) set
