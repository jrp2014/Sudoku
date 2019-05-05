{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module Sudoku where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Traversable
import NanoParsec

import FunctorCombo.Functor

import Data.Functor (($>))

type Triple = Id :*: Id :*: Id

pattern Tr :: a -> a -> a -> (:*:) (Id :*: Id) Id a

pattern Tr a b c = (Id a :*: Id b) :*: Id c

type Zone = Triple :. Triple

zone :: Zone Char
zone = O (Tr (Tr 'a' 'b' 'c') (Tr 'd' 'e' 'f') (Tr 'g' 'h' 'i'))

type Grid = Zone :. Zone

rows :: Grid a -> Grid a
rows = id

cols :: Grid a -> Grid a
cols = O . sequenceA . unO

boxs :: Grid a -> Grid a
--boxGrid = newly (fmap O . newly (fmap sequenceA) . fmap unO)
boxs = O . fmap O . O . fmap sequenceA . unO . fmap unO . unO

-- We can use Nothing, or 0 to represent unsolved values ...
type Value = Char

pboard :: Parser (Grid Value)
pboard = sequenceA (pure pcell)

pcell :: Parser Value
pcell =
  spaces *> ( digit <|> (char '.' $> '0')) <* spaces

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

test2 = valid . fst . head $ parse pboard tryThis



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

complete :: Grid Int -> Bool
complete = all (`elem` [1 .. 9])

valid :: Eq a => Grid a -> Bool
valid t = all (\f ->  nodups $ f t) [rows, cols, boxs]

nodups :: (Eq a, Foldable f) => f a -> Bool
nodups l = foldr test end l []
  where
    test :: Eq a => a -> ([a] -> Bool) -> [a] -> Bool
    test a cont seen = (a `elem` seen) || cont (a : seen)
    end :: [a] -> Bool
    end = const False
