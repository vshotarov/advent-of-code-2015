module Main where

import Data.Char (ord)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day19 --"
    input <- Common.readInput

    -- Parse
    let (startingMolecule,replacements) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString
                                  $ show (startingMolecule,replacements))

    -- Solve
    let toNewMolecules [] = []
        toNewMolecules xs = concat $ map expand [0..(length xs - 1)]
            where expand i = map (\v -> concat $ (take i xs) ++ [v] ++ (drop (i+1) xs))
                           $ case M.lookup (xs !! i) replacements of
                               Nothing -> []
                               Just vs -> vs
    let toElements molecule = go [] molecule
            where go [] [] = []
                  go buffer [] = [buffer]
                  go buffer (x:xs)
                    | isUpper x = case length buffer of
                                                   0 -> go [x] xs
                                                   _ -> buffer:(go [x] xs )
                    | otherwise                = go (buffer ++ [x]) xs
    let answer1 = length . Common.unique . toNewMolecules $ toElements startingMolecule
    {-
        I tried the recursive approach but that never finished and stayed around the
        140th step even with optimizations.

        Unfortunately, I had to look it up, but I am glad I did, as this solution
        is so elegant.

        https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4h7ji/
    -}
    let answer2 = numElements - (count "Rn") - (count "Ar") - 2 * (count "Y") - 1
            where numElements = length $ filter (isUpper) startingMolecule
                  count needle = count' needle startingMolecule
                      where count' _ [] = 0
                            count' y xs | isPrefixOf y xs = 1 + (count' y (drop (length y) xs))
                            count' y (_:xs) = count' y xs

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> (String, M.Map String [String])
parse input = (head startingMolecule, rules')
    where (rules,startingMolecule) = Common.splitOnceOn [""] $ lines input
          rules' = foldl folder M.empty rules
          folder acc x = M.insertWith (++) key [values] acc
              where (key,values) = Common.splitOnceOn " => " x

isUpper :: Char -> Bool
isUpper x = ord x > 64 && ord x < 97 
