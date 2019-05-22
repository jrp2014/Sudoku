{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Sudoku where

import Control.Applicative
import Data.Traversable
import NanoParsec

import FunctorCombo.Functor

import Data.Functor (($>))
import Data.List

-- Basic declarations
type Triple = Id :*: Id :*: Id

pattern Tr :: a -> a -> a -> (:*:) (Id :*: Id) Id a

pattern Tr a b c = (Id a :*: Id b) :*: Id c

type Row = Triple :. Triple

type Grid = Matrix Value

type Matrix = Row :. Row

type Value = Char

type Choices = [Value]

showValue :: Value -> Char
showValue = id

showChoices :: Choices -> String
--showChoices vs = '[' : vs ++ replicate (9 - length vs) ' ' ++ "]"
showChoices xs =
  (++ "]") .
  Data.List.foldl'
    (\acc x ->
       acc ++
       if x `elem` xs
         then [x]
         else " ")
    "[" $
  ['1' .. '9']

showTriple :: Triple Choices -> String
showTriple (Tr a b c) = showChoices a ++ showChoices b ++ showChoices c

showRow :: Row Choices -> String
showRow (O (Tr a b c)) = showTriple a ++ showTriple b ++ showTriple c ++ "\n"

showMatrix :: Matrix Choices -> String
showMatrix = foldMap showRow . unO

showMatrix' :: Matrix Choices -> String
showMatrix' = foldMap showChoices

-- putStrLn $ unlines $ fmap showRow $ sequenceA $ (fmap.fmap) (:[]) zone2
--
-- Basic definitions
values :: [Value]
values = ['1' .. '9']

-- Some format use 0, others '.', for blanks
blank :: Value -> Bool
blank x = x == '0' || x == '.'

single :: [a] -> Bool
single [_] = True
single _ = False

-- Extracting rows, columns and boxes
rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
-- cols = newly sequenceA
cols = O . sequenceA . unO

boxs :: Matrix a -> Matrix a
--boxMatrix = newly (fmap O . newly (fmap sequenceA) . fmap unO)
boxs = O . fmap O . O . fmap sequenceA . unO . fmap unO . unO

-- Parsing 
pboard :: Parser Grid
pboard = sequenceA (pure pcell)

pcell :: Parser Value
pcell = spaces *> (digit <|> (char '.' $> '0')) <* spaces

parseBoard :: String -> Grid
parseBoard = fst . head . parse pboard

-- Newtype coercion
newly :: (g1 (f1 a1) -> g2 (f2 a2)) -> (:.) g1 f1 a1 -> (:.) g2 f2 a2
newly f = O . f . unO

---------------
--
-- After the throat cleaing, onto the business
--
crush :: (Traversable f, Monoid b) => (a -> b) -> f a -> b
--crush f = getConst . traverse (Const . f)
crush = foldMap

reduce :: (Traversable t, Monoid o) => t o -> o
reduce = crush id

flatten :: (Traversable f) => f a -> [a]
flatten = crush (: [])

valid :: Grid -> Bool
valid g = all (\f -> nodups $ f g) [rows, cols, boxs]

-- TODO: is this the best we can do?
duplicates' :: (Eq a, Foldable f) => f a -> Bool
duplicates' l = foldr test end l []
  where
    test :: Eq a => a -> ([a] -> Bool) -> [a] -> Bool
    test a cont seen = (a `elem` seen) || cont (a : seen)
    end :: [a] -> Bool
    end = const False

duplicates :: (Traversable f, Eq a) => f a -> [a]
duplicates s = reduce $ snd $ mapAccumL gather [] s
  where
    gather seen current =
      if current `elem` seen
        then (seen, [current])
        else (current : seen, [])

nodups :: (Traversable f, Eq a) => f a -> Bool
nodups = null . duplicates

-- A basic solver
choices :: Grid -> Matrix Choices
choices = fmap choice
  where
    choice v =
      if blank v
        then values
        else [v]

-- Turn a grid of choices to a list of grids
cp :: Matrix Choices -> [Grid]
cp = sequenceA

collapse :: Matrix [a] -> [Matrix a]
collapse = sequenceA -- could be just collapse

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

-- Pruning the search space
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . newly (fmap thin) . f

thin :: Row Choices -> Row Choices
thin xss = fmap (`minus` singles xss) xss

singles :: Row Choices -> Choices
singles =
  foldMap
    (\a ->
       if single a
         then a
         else mempty)

minus :: Choices -> Choices -> Choices
xs `minus` ys =
  if single xs
    then xs
    else xs \\ ys

-- Repeatedly pruning
fix :: Eq a => (a -> a) -> a -> a
fix f x =
  if x == x'
    then x
    else fix f x'
  where
    x' = f x

-- Properties of Matrixes
-- Validity checking
complete :: Matrix Choices -> Bool
complete = all single

void :: Matrix Choices -> Bool
void = any null

safe :: Matrix Choices -> Bool
safe t = consistent (rows t) && consistent (cols t) && consistent (boxs t)

consistent :: Matrix Choices -> Bool
consistent = and . fmap consistentRow . unO

consistentRow :: Row Choices -> Bool
consistentRow = nodups . singles

blocked :: Matrix Choices -> Bool
blocked m = Sudoku.void m || not (safe m)

-- Makng choices one at a time
solve4 :: Grid -> [Grid]
solve4 = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | complete m = collapse m
  | otherwise = [g | m' <- expand m, g <- search (prune m')]

-- TODO: This doesn't work
expand :: Matrix Choices -> [Matrix Choices]
expand = traverse $ fmap (: [])

counts :: Row Choices -> Row Int
counts = fmap length

n :: Row Int -> Int
n = minimum

zone :: Row Char
zone = O (Tr (Tr 'a' 'b' 'c') (Tr 'd' 'e' 'f') (Tr 'g' 'h' 'a'))

zone2 :: Row [Value]
zone2 = O (Tr (Tr "ab" "bc" "c") (Tr "d" "e" "f") (Tr "g" "h" "ab"))

-- TODO: Put this into the test suite
tryThis :: String
tryThis =
  unlines
    [ "...23.6.."
    , "1.......7"
    , ".4...518."
    , "5.....9.."
    , "..73.68.."
    , "..4.....5"
    , ".867...5."
    , "4.......9"
    , "..3.62..."
    ]

easy :: String
easy =
  unlines
    [ "2....1.38"
    , "........5"
    , ".7...6..."
    , ".......13"
    , ".981..257"
    , "31....8.."
    , "9..8...2."
    , ".5..69784"
    , "4..25...."
    ]

test0 :: Grid
test0 = parseBoard easy

test1 :: [Value]
test1 = flatten test0

test2 :: Bool
test2 = valid test0

test3 :: String
test3 = showMatrix $ choices test0

test4 :: Matrix Choices
test4 = prune . prune $ choices test0

test5 :: IO ()
test5 = putStrLn . showMatrix . prune . prune $ choices test0

test6 = length (expand test4)
