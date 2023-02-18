module Main where

import Data.List (group)
import System.Environment

parseLasValue :: String -> (Integer, Integer)
parseLasValue n = ((toInteger $ length n), read $ take 1 n)

parseLasNumber :: String -> [(Integer, Integer)]
parseLasNumber = map parseLasValue . group

concatT :: (Integer, Integer) -> String
concatT (x, y) = show x ++ show y

lasSucc :: [(Integer, Integer)] -> [(Integer, Integer)]
lasSucc = parseLasNumber . concat . map concatT

printLasPair :: (Integer, Integer) -> String
printLasPair (x, y) = concat $ replicate (fromInteger x) $ show y

printLasNumber :: [(Integer, Integer)] -> String
printLasNumber = concat . map printLasPair

printLasSucc :: String -> String
printLasSucc = printLasNumber . lasSucc . parseLasNumber

lasNumbersAsStrings :: [String]
lasNumbersAsStrings = "1" : map printLasSucc lasNumbersAsStrings

lasNumbers :: [Integer]
lasNumbers = map read lasNumbersAsStrings

handleInput :: String -> Integer
handleInput = read . printLasNumber . lasSucc . parseLasNumber

main :: IO ()
main = do
  args <- getArgs
  print $ handleInput $ head args
