module Main where

import Data.List (permutations)
import qualified Data.Map as M
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day13 --"
    input <- Common.readInput

    -- Parse
    let (pairHappinessMap,everybody) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show pairHappinessMap)

    -- Solve
    let getPairHappiness (alice,bob) | M.member (alice,bob) pairHappinessMap =
            pairHappinessMap M.! (alice,bob) + pairHappinessMap M.! (bob,alice)
        getPairHappiness _ = 0 -- only case when this happens is when i am introduced
    let solve = maximum
              . map (sum . map getPairHappiness . toPairs)
              . permutations
            where toPairs names = zip names' $ tail names'
                      where names' = names ++ [head names]
    let answer1 = solve everybody
    let answer2 = solve $ "me":everybody

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> (M.Map (String,String) Int, [String])
parse input = (happinessMap, Common.unique names)
    where foldOne (a,delta,b) (map',names') = (M.insert (a,b) delta map', a:b:names')
          gainOrLose "gain" = 1
          gainOrLose "lose" = -1
          gainOrLose _ = error "Malformatted input"
          toTuple3 words' =
              (words' !! 0
               ,(read $ words' !! 3) * (gainOrLose $ words' !! 2)
               ,init $ last words') -- init to remove dot at the end
          (happinessMap,names) = foldr
                                   foldOne (M.empty,[])
                                 . map (toTuple3 . words)
                                 $ lines input
