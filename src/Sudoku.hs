{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Sudoku where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Monoid
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

zone :: Row Char
zone = O (Tr (Tr 'a' 'b' 'c') (Tr 'd' 'e' 'f') (Tr 'g' 'h' 'a'))

zone2 :: Row [Value]
zone2 = O (Tr (Tr "ab" "bc" "c") (Tr "d" "e" "f") (Tr "g" "h" "ab"))

type Grid = Matrix Value

type Matrix = Row :. Row

type Value = Char

showValue :: Value -> Char
showValue = id

showCell :: [Value] -> String
showCell vs = '[' : vs ++ replicate (9 - length vs) ' ' ++ "]"

showTriple :: Triple [Value] -> String
showTriple (Tr a b c) = showCell a ++ showCell b ++ showCell c

showRow :: Row [Value] -> String
showRow (O (Tr a b c)) = showTriple a ++ showTriple b ++ showTriple c

showMatrix :: Matrix [Value] -> String
showMatrix = undefined

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
cols = O . sequenceA . unO

boxs :: Matrix a -> Matrix a
--boxMatrix = newly (fmap O . newly (fmap sequenceA) . fmap unO)
boxs = O . fmap O . O . fmap sequenceA . unO . fmap unO . unO

-- Parsing 
pboard :: Parser Grid
pboard = sequenceA (pure pcell)

pcell :: Parser Value
pcell = spaces *> (digit <|> (char '.' $> '0')) <* spaces

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

test1 :: [Value]
test1 = flatten . fst . head $ parse pboard tryThis

test2 :: Bool
test2 = safe . fst . head $ parse pboard tryThis

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

-- TODO: is this the best we can do?
nodups :: (Eq a, Foldable f) => f a -> Bool
nodups l = foldr test end l []
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

-- A basic solver
type Choices = [Value]

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
collapse = sequenceA

solve :: Grid -> [Grid]
solve = filter safe . collapse . choices

-- Pruning the search space
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . newly (fmap thin) . f

thin :: Row Choices -> Row Choices
thin xss = fmap (`minus` singles) xss
  where
    singles :: Choices
    singles =
      foldMap
        (\a ->
           if single a
             then a
             else mempty)
        xss

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

safe :: Eq a => Matrix a -> Bool
safe t = all (\f -> nodups $ f t) [rows, cols, boxs]

valid :: Grid -> Bool
valid = safe

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

expand = undefined

counts :: Row Choices -> Row Int
counts = fmap length

n :: Row Int -> Int
n = minimum
