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

type Board = Zone :. Zone

-- We can use Nothing, or 0 to represent unsolved values ...
type Cell = Maybe Int

pboard :: Parser (Board Cell)
pboard = sequenceA (pure pcell)

pcell :: Parser Cell
pcell = spaces *> ((Just <$> singleDigitInt) <|> (char '.' $> Nothing)) <* spaces

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
