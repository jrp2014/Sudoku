{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- OPTIONS_GHC -ddump-deriv
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes -}


-- Like Types 4, but using GHC (Generics) to generate many of the instances and
-- some of the Functor Kit
-- We could also replace some of the Newtype giggery pokery with
-- @Control.NewType@ machinery.

{-----------------------------------------------------------------------------}
module Generics where

-- Adapted from
-- https://github.com/pigworker/WhatRTypes4/blob/master/Types4Crib.hs

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Foldable (Foldable (fold))
import Data.Functor (($>))
import Data.List (mapAccumL, nub, (\\))
import Data.Monoid (All (..), Any (..), Product (..), Sum (..))
import GHC.Generics
--import Control.Newtype

{-----------------------------------------------------------------------}
-- a parser for things is...                                 (Fritz Ruehr)
-- See also http://dev.stephendiehl.com/fun/002_parsers.html

newtype P thing = P {parse :: String -> [(thing, String)]}
  deriving stock (Functor)
  deriving newtype (Semigroup, Monoid)

instance Applicative P where
  pure x = P $ \s -> [(x, s)]
  pf <*> px = P $ \s -> [(f x, sx) | (f, sf) <- parse pf s, (x, sx) <- parse px sf]

instance Monad P where
  return x = P $ \s -> [(x, s)]
  p >>= f = P $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

{-
P af >>= k = P $ \ s -> do
  (a, s) <- af s
  parse (k a) s
-}

instance Alternative P where -- (what if P used Maybe?)
  empty = mempty
  (<|>) = mappend

eat :: (Char -> Bool) -> P Char
eat p = P $ \case
  (c : s) | p c -> [(c, s)]
  _ -> []

type Cell = Maybe Int

{---------------------------------------------------------------------}
-- the functor kit

newtype I x = I x 
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving newtype (Show, Semigroup, Monoid)

newtype K a x = K {unK :: a}
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving newtype (Show, Semigroup, Monoid)

instance Applicative I where
  pure = I
  I f <*> I s = I (f s)

instance Monoid a => Applicative (K a) where
  pure _x = K mempty
  K f <*> K s = K (mappend f s)

{---------------------------------------------------------------------}
-- Newtype piggery-jokery

class Newtype new where
  type Old new
  pack :: Old new -> new
  unpack :: new -> Old new

newly :: (Newtype a, Newtype b) => (Old a -> Old b) -> a -> b
newly f = pack . f . unpack

ala :: (Newtype b, Newtype d) => ((a -> b)     -> c -> d) -> (Old b -> b)
                               -> (a -> Old b) -> c -> Old d
ala hof _ f = unpack . hof (pack . f)

infixl `ala`

instance Newtype ((f :.: g) x) where
  type Old ((f :.: g) x) = f (g x)
  pack = Comp1
  unpack = unComp1

instance Newtype (K a x) where
  type Old (K a x) = a
  pack = K
  unpack (K a) = a

instance Newtype (I x) where
  type Old (I x) = x
  pack = I
  unpack (I x) = x

instance Newtype (Product a) where
  type Old (Product a) = a
  pack = Product
  unpack = getProduct

instance Newtype (Sum a) where
  type Old (Sum a) = a
  pack = Sum
  unpack = getSum

instance Newtype Any where
  type Old Any = Bool
  pack = Any
  unpack = getAny

instance Newtype All where
  type Old All = Bool
  pack = All
  unpack = getAll


{---------------------------------------------------------------------}
-- triples of triples, and their transposes

type Triple = (I :*: I) :*: I

pattern Tr :: x -> x -> x -> (:*:) (I :*: I) I x
pattern Tr a b c = (I a :*: I b) :*: I c

type Zone = Triple :.: Triple

-- what's for free?

zone :: Zone Char
zone =  Comp1 (Tr (Tr 'a' 'b' 'c') (Tr 'd' 'e' 'f') (Tr 'g' 'h' 'i'))

{-----------------------------------------------------------------------}

type Board = Zone :.: Zone

--pboard :: P (Board Cell)
--pboard = sequenceA (pure pcell)

-- transpose a board
xpBoard :: Board c -> Board c
xpBoard =  newly sequenceA

-- turn each box into a board row
boxBoard :: Board c -> Board c
boxBoard = newly (fmap Comp1 . newly (fmap sequenceA) . fmap unComp1)

crush :: (Traversable f, Monoid b) => (a -> b) -> f a -> b
crush = foldMap -- unK . traverse (K . f) -- foldMap

reduce :: (Traversable t, Monoid o) => t o -> o
reduce = fold -- foldMap id -- fold / cf: mconcat

flatten :: (Traversable f) => f a -> [a]
flatten = foldMap (: [])

ok :: Board Int -> Bool
ok board = all (null . duplicates . ($ board)) [id, xpBoard, boxBoard]

-- For testing only.  The real main is in app/Main.hs
main :: IO ()
main = do
  putStrLn $ solve5 tryThis
  putStrLn $ solve5 easy
  putStrLn $ solve5 gentle
  putStrLn $ solve5 diabolical
  putStrLn $ solve5 unsolvable

--  putStrLn $ solve5 minimal
--  putStrLn $ solve5 nefarious

-- Basic types

-- This representation of Suduko grids uses the funcor kit rather than lists
type Grid = Matrix Value

type Matrix = Row :.: Row

type Row = Zone

type Value = Char

type Choices = [Value]

-- Pretty printing

class Show a => Display a where
  display :: a -> String

instance Show a => Display (Maybe a) where
  display Nothing = "."
  display (Just a) = show a

instance Display a => Display [a] where
  display xs = "[" ++ concatMap display xs ++ "]"

instance Display Int where
  display = show

instance Display Char where
  display c = show (digitToInt c)

instance Display a => Display (I a) where
  display (I x) = display x

instance Display a => Display (K a x) where
  display k = display (K k)

instance (Display (f (g a))) => Display ((f :.: g) a) where
  display = (<> "\n") . display . unComp1

instance (Display (f a), Display (g a)) => Display ((f :*: g) a) where
  display (f :*: g) = display f <> display g

-- Helper functions

values :: [Value]
values = ['1' .. '9']

-- Some formats use 0, others '.', for empties
iSempty :: Value -> Bool
iSempty x = x == '0' || x == '.'

single :: [a] -> Bool
single [_] = True
single _ = False

pair :: [a] -> Bool
pair [_, _] = True
pair _ = False

treble :: [a] -> Bool
treble [_, _, _] = True
treble _ = False

-- Extracting rows, columns and boxes
rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols = newly sequenceA

boxs :: Matrix a -> Matrix a
boxs = newly (fmap Comp1 . newly (fmap sequenceA) . fmap unComp1)

-- Parsing
pboard :: P Grid
pboard = sequenceA (pure pcell)

pcell :: P Value
pcell = many (eat isSpace) *> (eat isDigit <|> (eat (== '.') $> '0'))

parseBoard :: String -> Grid
parseBoard s = b
  where
    [(b, _)] = parse pboard s

---------------
--
-- After the throat cleaing, onto the business
--

-- TODO: is this the best we can do?
duplicates' :: (Eq a, Foldable f) => f a -> Bool
duplicates' l = foldr test end l []
  where
    test :: Eq a => a -> ([a] -> Bool) -> [a] -> Bool
    test a cont seen = (a `elem` seen) || cont (a : seen)
    end :: [a] -> Bool
    end = const False

duplicates :: (Traversable f, Eq a) => f a -> [a]
duplicates s = nub . reduce . snd $ mapAccumL gather [] s
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
      if iSempty v
        then values -- 1-9
        else [v]

occurrences :: Foldable f => Choices -> f Choices -> Int
occurrences cs =
  getSum
    . foldMap
      ( \a ->
          if a == cs
            then Sum 1
            else mempty
      )

nonEmptySubsets :: (Eq a) => [a] -> [[a]]
nonEmptySubsets = tail . subsets'
  where
    subsets' :: (Eq a) => [a] -> [[a]]
    subsets' [] = [[]]
    subsets' (x : xs) = subsets' xs ++ map (x :) (subsets' xs)

filterRow :: (a -> Bool) -> Row a -> [a]
filterRow p = foldMap (\x -> [x | p x])

countSatisfy :: (a -> Bool) -> Row a -> Int
countSatisfy p =
  getSum
    . foldMap
      ( \x ->
          Sum $
            if p x
              then 1
              else 0
      )

--counts p = getSum . foldMap (\x -> if p x then 1 else 0)
isSubsetOf :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
isSubsetOf a b = all (`elem` b) a

--
-- Turn a grid of choices to a list of grids
cp :: Matrix Choices -> [Grid]
cp = sequenceA

collapse :: Matrix [a] -> [Matrix a]
collapse = sequenceA

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

-- Pruning the search space
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . newly (fmap thin) . f

prune2 :: Matrix Choices -> Matrix Choices
prune2 = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . newly (fmap thin2) . f

prune3 :: Matrix Choices -> Matrix Choices
prune3 = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . newly (fmap thin3) . f

thin :: Row Choices -> Row Choices
thin xss = fmap (`minus` singles xss) xss

thin2 :: Row Choices -> Row Choices
thin2 xss
  | null duplicatePairs = xss
  | otherwise = fmap (`minus2` duplicatePairs) xss
  where
    thePairs = pairs xss
    duplicatePairs = duplicates thePairs

thin3 :: Row Choices -> Row Choices
thin3 xss
  | null tripleTrebles = xss
  | otherwise = fmap (`minus2` tripleTrebles) xss
  where
    theTrebles = trebles xss
    tripleTrebles =
      nub $ filter (\trble -> occurrences trble theTrebles == 3) theTrebles

singles :: Row Choices -> Choices
singles =
  foldMap
    ( \a ->
        if single a
          then a
          else mempty
    )

pairs :: Row Choices -> [Choices]
pairs =
  foldMap
    ( \a ->
        if pair a
          then [a]
          else mempty
    )

trebles :: Row Choices -> [Choices]
trebles =
  foldMap
    ( \a ->
        if treble a
          then [a]
          else mempty
    )

minus :: Choices -> Choices -> Choices
xs `minus` ys =
  if single xs
    then xs
    else xs \\ ys

minus2 :: Choices -> [Choices] -> Choices
xs `minus2` (ys : yss)
  | xs == ys = xs `minus2` yss
  | otherwise = (xs \\ ys) `minus2` yss
xs `minus2` [] = xs

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

valid :: Grid -> Bool
valid g = all (\f -> nodups $ f g) [rows, cols, boxs]

void :: Matrix Choices -> Bool
void = any null

safe :: Matrix Choices -> Bool
safe cm = consistent (rows cm) && consistent (cols cm) && consistent (boxs cm)

consistent :: Matrix Choices -> Bool
consistent = and . fmap consistentRow . unComp1

consistentRow :: Row Choices -> Bool
consistentRow = nodups . singles

blocked :: Matrix Choices -> Bool
blocked m = Generics.void m || not (safe m)

-- Makng choices one at a time
solve4 :: Grid -> [Grid]
solve4 = search . prune . choices

--solve4 = search . prune . prune2 . prune3 . choices

-- Use solve4 to solve the given board
solve5 :: String -> String
solve5 input
  | length results == 1 = head results
  | otherwise = "No unique solution found:\n" ++ show results
  where
    results = map display . solve4 . parseBoard $ input

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | complete m = collapse m
  | otherwise = [g | m' <- expand m, g <- search (prune m')]

choose :: Choices -> StateT Bool [] Choices
choose [x] = return [x]
choose xss = do
  chosen <- get
  if chosen
    then return xss
    else do
      put True
      x <- lift xss
      return [x]

expand :: Matrix Choices -> [Matrix Choices]
--expand :: Traversable t => t [a] -> [t [a]]
expand xs = evalStateT (traverse choose xs) False

counts :: Row Choices -> Row Int
counts = fmap length

-- TODO: Put this into the test suite
tryThis :: String
tryThis =
  unlines
    [ "...23.6..",
      "1.......7",
      ".4...518.",
      "5.....9..",
      "..73.68..",
      "..4.....5",
      ".867...5.",
      "4.......9",
      "..3.62..."
    ]

easy :: String
easy =
  unlines
    [ "2....1.38",
      "........5",
      ".7...6...",
      ".......13",
      ".981..257",
      "31....8..",
      "9..8...2.",
      ".5..69784",
      "4..25...."
    ]

gentle :: String
gentle =
  unlines
    [ ".1.42...5",
      "..2.71.39",
      ".......4.",
      "2.71....6",
      "....4....",
      "6....74.3",
      ".7.......",
      "12.73.5..",
      "3...82.7."
    ]

--First diabolical example:
diabolical :: String
diabolical =
  unlines
    [ ".9.7..86.",
      ".31..5.2.",
      "8.6......",
      "..7.5...6",
      "...3.7...",
      "5...1.7..",
      "......1.9",
      ".2.6..35.",
      ".54..8.7."
    ]

-- First "unsolvable" (requires backtracking) example:
unsolvable :: String
unsolvable =
  unlines
    [ "1..9.7..3",
      ".8.....7.",
      "..9...6..",
      "..72.94..",
      "41.....95",
      "..85.43..",
      "..3...7..",
      ".5.....4.",
      "2..8.6..9"
    ]

--Minimal sized grid (17 values) with a unique solution:
minimal :: String
minimal =
  unlines
    [ ".98......",
      "....7....",
      "....15...",
      "1........",
      "...2....9",
      "...9.6.82",
      ".......3.",
      "5.1......",
      "...4...2."
    ]

nefarious :: String
nefarious =
  unlines
    [ "....6..8.",
      ".2.......",
      "..1......",
      ".7....1.2",
      "5...3....",
      "......4..",
      "..42.1...",
      "3..7..6..",
      ".......5."
    ]
