module Common ( readInput
              , truncateString
              , flattenTuples2
              , everyNth
              , firstWhere
              , firstIdWhere
              ) where

import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)

readInput :: IO String
readInput = do
    args <- getArgs
    case args of
      [arg]              -> do
          fileExists <- doesFileExist arg
          if fileExists
          then putStrLn "-- Interpreting argument as a file" >> readFile' arg
          else putStrLn "-- Interpreting argument as a string" >> return arg
      ["--fromFile",arg] -> readFile' arg
      ["--raw",arg]      -> return arg
      _                  -> do
          progName <- getProgName
          let inputFilePath = progName ++ "/input.txt"
          putStrLn $ "-- No arg passed. Reading " ++ inputFilePath
          readFile' inputFilePath

readFile' :: String -> IO String
readFile' fp = do
    fileData <- readFile fp
    let asLines = lines fileData
    if length asLines == 1
       then putStrLn "-- Stripping \\n at the end, as the input is one line long"
            >> (return $ head asLines)
       else return fileData

truncateString :: String -> String
truncateString str | length str < 70 = str
                   | otherwise       = take 53 str ++ " ... "
                                    ++ (drop (length str - 13) str)

flattenTuples2 :: [(a,a)] -> [a]
flattenTuples2 []          = []
flattenTuples2 ((x,y):xys) = x:y:flattenTuples2 xys

everyNth :: Int -> [a] -> [a]
everyNth _ []     = []
everyNth n (x:xs) = x:(everyNth n (drop (n-1) xs))

firstWhere :: (a -> Bool) -> [a] -> a
firstWhere _ [] = error "firstWhere: on empty list"
firstWhere predicament (x:_) | predicament x = x
firstWhere predicament (_:xs)                = firstWhere predicament xs

firstIdWhere :: (a -> Bool) -> [a] -> Int
firstIdWhere _ [] = error "firstIdWhere: on empty list"
firstIdWhere predicament xs = go xs 0
    where go [] _ = error "firstIdWhere: no elements satisfy predicament"
          go (x:_) n | predicament x = n
          go (_:xs') n               = go xs' (n+1)
