module Main where

import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy (ByteString, pack)
import Data.Char (ord)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput
    putStrLn $ "Input: " ++ (Common.truncateString $ show input)

    -- Solve
    let intsToMD5Hashes :: [Int] -> [(Int,String)]
        intsToMD5Hashes ints =
            map (\i -> (i,show $ md5 $ toLazyByteString (input ++ show i))) ints

    let answer1 = Common.firstWhere ((=="00000") . take 5 . snd)
                $ intsToMD5Hashes [0..]
    let answer2 = Common.firstWhere ((=="000000") . take 6 . snd)
                $ intsToMD5Hashes [(fst answer1)..]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

toLazyByteString :: String -> ByteString
toLazyByteString = pack . map (fromIntegral . ord)
