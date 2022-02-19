module Main where

import qualified Data.Map as M
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day16 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let rules =  [("children",3)
                 ,("cats",7)
                 ,("samoyeds",2)
                 ,("pomeranians",3)
                 ,("akitas",0)
                 ,("vizslas",0)
                 ,("goldfish",5)
                 ,("trees",3)
                 ,("cars",2)
                 ,("perfumes",1)]
    let predicaments1 = map (\(k,v) -> (k,(==v))) rules
    let predicaments2 = map (\(k,v) ->
                              (k,(case v of
                                    _ | k `elem` ["cats","trees"] -> (>v)
                                    _ | k `elem` ["pmeranians","goldfish"] -> (<v)
                                    _ -> (==v)))) rules
    let solve predicaments = (+1) . fst . head
                           $ filter (isAunt predicaments . snd) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show (solve predicaments1)
    putStrLn $ "Part 2: " ++ show (solve predicaments2)

type Aunt = M.Map String Int
parse :: String -> [(Int,Aunt)]
parse input = zip [0..] . map parseOne $ lines input
    where parseOne line = foldl
                            (\acc (n,v) -> M.insert n (read v) acc)
                            M.empty pairs
              where words' = drop 2 . words $ filter (not . (`elem` ":,")) line
                    pairs = Common.everyNth 2 . zip words' $ tail words'

type Rule = (String,(Int -> Bool))
isAunt :: [Rule] -> Aunt -> Bool
isAunt rules aunt =
    all (\(k,predicament) -> maybe True predicament $ M.lookup k aunt) rules
