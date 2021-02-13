-- Copyright (c) 2007 Wouter Swierstra
-- All rights reserved.
--
-- Based on Graham Hutton's version of Richard Bird's Sudoku solver.
module Main where

import Data.List
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [TVar a]
type Value            = Char

-- Some pure amenities

-- The size of the board
boxsize :: Int
boxsize =  3

-- The possible values of a cell
values :: [Value]
values =  ['1'..'9']

-- A dummy value representing the empty cell
empty :: Value -> Bool
empty =  (== '.')

-- When is a cell filled in or not
single     :: [a] -> Bool
single [_] = True
single _   = False

-- Some functions that return a list of nine rows, columns, or
-- boxes of a grid.
chop      :: Int -> [a] -> [[a]]
chop n [] =  []
chop n xs =  take n xs : chop n (drop n xs)

rows :: Matrix a -> [Row a]
rows =  id

cols :: [[a]] -> [[a]]
cols =  transpose

boxes :: [[a]] -> [[a]]
boxes =  unpack . map cols . pack
  where
    pack   = split . map split
    split  = chop boxsize
    unpack = map concat . concat

-- When does a list have no duplicates
nodups        :: Eq a => [a] -> Bool
nodups []     =  True
nodups (x:xs) =  not (elem x xs) && nodups xs

-- collapse takes a Grid where every cell contains a list of
-- possibilities, to a list of Grids where every cell contains a
-- single value.
collapse :: [[[a]]] -> [[[a]]]
collapse =  cp . map cp

-- cartesian product of a list of lists
cp          :: [[a]] -> [[a]]
cp []       =  [[]]
cp (xs:xss) =  [y:ys | y <- xs, ys <- cp xss]

-- The choices function reads in a Sudoku grid, replacing each
-- unknown entry by a TVar containing ['1' .. '9'] and each fixed
-- entry x by a TVar containing [x].
type Choices          =  [Value]

choices               :: [[Value]] -> STM (Matrix Choices)
choices vs            =  mapM (mapM choice) vs

choice :: Value -> STM (TVar [Value])
choice v = do
  newTVar $
    if empty v
    then values
    else return v

-- find all the digits that have been filled in
findSingles :: Row Choices -> STM [Value]
findSingles [] = return []
findSingles (xs:xss) = do
  v <- readTVar xs
  ss <- findSingles xss
  if single v then return (v ++ ss)
              else return ss

-- cross off all the digits that have been filled in
reduce :: Row Choices -> STM ()
reduce row = do
  singles <- findSingles row
  mapM_ (removeSingles singles) row

removeSingles :: Choices -> TVar Choices -> STM ()
removeSingles singles var = do
  v <- readTVar var
  writeTVar var (v `minus` singles)

-- the prune function prunes the search space, e.g. removing '9'
-- from the cells in a row/column/box if there is already a cell
-- with a '9' in said row/column/box. Using STM makes the
-- concurrency here quite neat - we can prune the rows, columns, and
-- boxes at the same time.
prune :: Matrix Choices -> IO ()
prune ms = do
  rowsDone <- newEmptyMVar
  colsDone <- newEmptyMVar
  boxesDone <- newEmptyMVar
  forkIO (pruneBy rowsDone rows ms)
  forkIO (pruneBy colsDone cols ms)
  forkIO (pruneBy boxesDone boxes ms)
  takeMVar rowsDone
  takeMVar colsDone
  takeMVar boxesDone

pruneBy :: MVar () -> (Matrix Choices -> Matrix Choices) 
  -> Matrix Choices -> IO ()
pruneBy mvar f m = do
  atomically $ mapM_ reduce (f m)
  putMVar mvar ()


-- When is a matrix completely filled in?
complete              :: Matrix Choices -> STM Bool
complete m            =  liftM (all (all single)) (mapM (mapM readTVar) m)

-- When are we 'stuck', i.e. when there is a cell with no possible
-- choices left.
void                  :: Matrix Choices -> STM Bool
void m                =  liftM (any (any null)) (mapM (mapM readTVar) m)

minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs \\ ys

-- A board is consistent if there are no duplicates in every row,
-- column, and box.
isInconsistent    :: Matrix Choices -> STM Bool
isInconsistent cm = do
  rowC <- liftM (all consistent) (mapM (mapM readTVar) (rows cm))
  colC <- liftM (all consistent) (mapM (mapM readTVar) (cols cm))
  boxC <- liftM (all consistent) (mapM (mapM readTVar) (boxes cm))
  return (not (rowC && colC && boxC))

consistent            :: [[Value]] -> Bool
consistent            =  nodups . concat . filter single

-- A board is blocked if it is void or inconsistent
blocked               :: Matrix Choices -> STM Bool
blocked m             =  liftM2 (||) (void m) (isInconsistent m)

-- The search function checks 
--
-- * if the board is blocked, we cannot make any progress in this
-- thread
--
-- * if the board is complete, we are done and fill in the MVar
-- waiting for the result.
--
-- * otherwise, expand the cell with the smallest number of
-- remaining choices to make a list of boards, corresponding to the
-- possible ways to fill in that cell. We then fork off a thread to
-- try and find a solution for every board in that list.
search :: MVar [[Value]] -> Matrix Choices -> IO ()
search mvar m = do
  isBlocked <- atomically $ blocked m
  isComplete <- atomically $ complete m
  if isBlocked
    then return ()
    else
      if isComplete
      then do
        result <- atomically $ liftM collapse (mapM (mapM readTVar) m)
        putMVar mvar (head result)
      else do
        ms <- expand m
        mapM_ (\m -> forkIO (prune m >> search mvar m)) ms

expand :: Matrix Choices -> IO ([Matrix Choices])
expand matrix = do
  ms <- atomically $ mapM (mapM readTVar) matrix
  let mms = expand' ms
  atomically $ mapM (mapM (mapM newTVar)) mms
  
expand'                :: [[Choices]] -> [[[Choices]]]
expand' m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any p) m
      (row1,cs:row2)    = break p row
      p xs              = length xs == minLength
      minLength         = minimum (filter (> 1) (concatMap (map length) m))


-- The solve function makes an empty MVar, reads in the board,
-- prunes it, and searches for solutions. Once a solution is found,
-- it will be written to the MVar and printed to stdout.
solve :: [[Value]] -> IO ()
solve grid = do
  solution <- newEmptyMVar
  matrix <- atomically $ choices grid
  prune matrix
  search solution matrix
  s <- takeMVar solution
  putStrLn (unlines s)

-- Examples
easy                  :: [[Value]]
easy                  =  ["2....1.38",
                          "........5",
                          ".7...6...",
                          ".......13",
                          ".981..257",
                          "31....8..",
                          "9..8...2.",
                          ".5..69784",
                          "4..25...."]
gentle                :: [[Value]]
gentle                =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]
diabolical            :: [[Value]]
diabolical            =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]
unsolvable            :: [[Value]]
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4.",
                          "2..8.6..9"]
minimal               :: [[Value]]
minimal               =  [".98......",
                          "....7....",
                          "....15...",
                          "1........",
                          "...2....9",
                          "...9.6.82",
                          ".......3.",
                          "5.1......",
                          "...4...2."]

main = solve minimal