{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module Sudoku where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Traversable
import NanoParsec

import FunctorCombo.DHoley
import FunctorCombo.Functor

import Data.Functor (($>))

type Triple = Id :*: Id :*: Id

pattern Tr :: a -> a -> a -> (:*:) (Id :*: Id) Id a

pattern Tr a b c = (Id a :*: Id b) :*: Id c

type Zone = Triple :. Triple

zone :: Zone Char
zone = O (Tr (Tr 'a' 'b' 'c') (Tr 'd' 'e' 'f') (Tr 'g' 'h' 'i'))

type Board = Zone :. Zone

rows :: Board a -> Board a
rows = id

cols :: Board a -> Board a
cols = O . sequenceA . unO

boxes :: Board a -> Board a
--boxBoard = newly (fmap O . newly (fmap sequenceA) . fmap unO)
boxes = O . fmap O . O . fmap sequenceA . unO . fmap unO . unO

-- We can use Nothing, or 0 to represent unsolved values ...
type Cell = Maybe Int

pboard :: Parser (Board Cell)
pboard = sequenceA (pure pcell)

pcell :: Parser Cell
pcell =
  spaces *> ((Just <$> singleDigitInt) <|> (char '.' $> Nothing)) <* spaces

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

-- Newtype coercion
newly :: (g1 (f1 a1) -> g2 (f2 a2)) -> (:.) g1 f1 a1 -> (:.) g2 f2 a2
newly f = O . f . unO

xpBoard :: Board Cell -> Board Cell
xpBoard = newly sequenceA

boxBoard :: Board Cell -> Board Cell
boxBoard = newly (fmap O . newly (fmap sequenceA) . fmap unO)

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

--elem :: (Eq a, Traversable t) => a -> t a -> Bool
--elem = any . (==)
complete :: Board Int -> Bool
complete = all (`elem` [1 .. 9])

type DBoard = Der Board
