module Main where

import Sudoku hiding (main)


main :: IO ()
main = do
  putStrLn $ solve5 tryThis
  putStrLn $ solve5 easy
  putStrLn $ solve5 gentle
  putStrLn $ solve5 diabolical
  putStrLn $ solve5 unsolvable
  putStrLn $ solve5 minimal
  putStrLn $ solve5 nefarious
