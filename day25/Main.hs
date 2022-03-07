module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day25 --"

    -- Solve
    let i = snd . Common.firstWhere ((==(3010,3019)) . fst)
          $ iterate (step (+1)) ((1,1),1)
    let answer1 = snd . (!! (i-1))
                $ iterate (step ((`mod` 33554393) . (*252533))) ((1,1),20151125)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1

step :: (Int -> Int) -> ((Int,Int),Int) -> ((Int,Int),Int)
step f ((row,col),x)
  | row == 1 = ((col+1,1),f x)
  | col == 1 = ((row-1,2),f x)
  | otherwise = ((row-1,col+1),f x)
