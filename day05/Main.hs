module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length
                $ filter (\x -> numVowels x >= 3
                             && hasDouble x
                             && (not $ containsNaughty x))
                         parsedInput

    let answer2 = length
                $ filter (\x -> containsRepeatingPair x
                             && containsRepeatingCharWithGap x)
                         parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

numVowels :: String -> Int
numVowels [] = 0
numVowels (x:xs) = (if x `elem` "aeiou" then 1 else 0) + numVowels xs

hasDouble :: String -> Bool
hasDouble [] = False
hasDouble [_] = False
hasDouble (x:y:_) | x == y = True
hasDouble (_:xs) = hasDouble xs

containsNaughty :: String -> Bool
containsNaughty [] = False
containsNaughty [_] = False
containsNaughty (x:y:_) | [x,y] `elem` ["ab","cd","pq","xy"] = True
containsNaughty (_:xs) = containsNaughty xs

containsRepeatingPair :: String -> Bool
containsRepeatingPair [] = False
containsRepeatingPair [_,_] = False
containsRepeatingPair (x:y:xys) | (x,y) `elem` (zip xys (tail xys)) = True
containsRepeatingPair (_:xs) = containsRepeatingPair xs

containsRepeatingCharWithGap :: String -> Bool
containsRepeatingCharWithGap [] = False
containsRepeatingCharWithGap [_] = False
containsRepeatingCharWithGap [_,_] = False
containsRepeatingCharWithGap (x:_:y:_) | x == y = True
containsRepeatingCharWithGap (_:xs) = containsRepeatingCharWithGap xs
