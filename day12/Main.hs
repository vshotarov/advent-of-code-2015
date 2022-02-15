module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = jsonSum $ fmap (\x -> if x=="red" then "0" else x) parsedInput
    let answer2 = jsonSum parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data JSON a = Object [JSON a]
          | List [JSON a]
          | Term a
          deriving (Show,Eq)

instance Functor JSON where
    fmap f (Term a) = Term $ f a
    fmap f (List as) = List $ fmap (fmap f) as
    fmap f (Object as) = Object $ fmap (fmap f) as

parse :: String -> JSON String
parse = go [] []
    where go [] [] ('[':xs)                  = go ['['] [List []] xs
          go [] [] ('{':xs)                  = go ['{'] [Object []] xs
          go stack buffer ('[':xs)           = go ('[':stack) ((List []):buffer) xs
          go stack buffer ('{':xs)           = go ('{':stack) ((Object []):buffer) xs
          go ['['] [b] [']']                 = b
          go ['{'] [b] ['}']                 = b
          go ('[':stack) (b:u:ffer) (']':xs) = go stack ((jsonInsert b u):ffer) xs
          go ('{':stack) (b:u:ffer) ('}':xs) = go stack ((jsonInsert b u):ffer) xs
          -- recognized values are numbers and "red"
          go stack (b:uffer) xs | isPrefixOf ":\"red\"" xs =
              go stack ((jsonInsert (Term "red") b):uffer) (drop 6 xs)
          go stack (b:uffer) xs = go stack (b':uffer) xs'
              where digitsPrefix = takeWhile (\x -> isDigit x || x=='-') xs
                    (b',xs') = case length digitsPrefix of
                                 0 -> (b,drop 1 xs)
                                 n -> (jsonInsert (Term digitsPrefix) b,drop n xs)
          go stack buffer xs = error $ "Unexpected error: " ++ show(stack,buffer,xs)

jsonInsert :: JSON String -> JSON String -> JSON String
jsonInsert v (Object values) = Object (v:values)
jsonInsert v (List values) = List (v:values)
jsonInsert v (Term t) = error $ "jsonInsert: Can't insert into Term" ++ show (v,t)

jsonSum :: JSON String -> Int
jsonSum (Term "red") = 0
jsonSum (Term x) = read x
jsonSum (List xs) = sum $ map jsonSum xs
jsonSum (Object xs) | Term "red" `elem` xs = 0
jsonSum (Object xs) = sum $ map jsonSum xs
