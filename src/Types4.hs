{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-----------------------------------------------------------------------------}
module Types4 where

-- Adapted from
-- https://github.com/pigworker/WhatRTypes4/blob/master/Types4Crib.hs

import Control.Applicative ( Alternative((<|>), empty, many) )
import Data.Char ( isDigit, isSpace )
import Data.Monoid ( All(..), Any(..), Product(..), Sum(..) )
import Data.Foldable (fold)
--import Data.Traversable
--import Data.Functor (($>))

{-----------------------------------------------------------------------}
-- a parser for things is...                                 (Fritz Ruehr)

-- See also http://dev.stephendiehl.com/fun/002_parsers.html

newtype P thing = P {parse :: String -> [(thing, String)]} deriving (Semigroup, Monoid)

instance Functor P where
  fmap f (P p) = P $ \s -> [(f a, b) | (a, b) <- p s]

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

pcell :: P Cell
pcell =
  many (eat isSpace)
    *> ( Just . read <$> ((:) <$> eat isDigit <*> pure [])
           <|> Nothing <$ eat (== '.')
       )

{---------------------------------------------------------------------}
-- the functor kit

-- Also implemented at
-- import FunctorCombo.Functor
-- import FunctorCombo.DHoley
-- import FunctorCombo.ZipperFix

newtype I x = I x deriving (Show)

newtype K a x = K { unK :: a } deriving (Show)

data (f :*: g) x = f x :*: g x deriving (Show)

data (f :+: g) x = L (f x) | R (g x) deriving (Show)

newtype (f :.: g) x = C {unC :: f (g x)} deriving (Show)

instance Functor I where
  fmap f (I x) = I $ f x

instance Applicative I where
  pure = I
  I f <*> I s = I (f s)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure x = pure x :*: pure x
  (ff :*: gf) <*> (fs :*: gs) = (ff <*> fs) :*: (gf <*> gs)

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = C . pure . pure
  C fgf <*> C fgs = C ((<*>) <$> fgf <*> fgs)

instance Functor (K a) where
  fmap _ (K x) = K x

instance Monoid a => Applicative (K a) where
  pure _x = K mempty
  K f <*> K s = K (mappend f s)

instance (Foldable g, Foldable f, Functor g) => Foldable (g :.: f) where
  foldMap f = foldMap (foldMap f) . unC

instance Foldable I where
  foldMap f (I a) = f a

instance Foldable (K a) where
  foldMap _ _ = mempty

instance Traversable (K a) where
  traverse _ (K a) = pure $ K a

instance Traversable I where
  traverse f (I x) = I <$> f x

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap k (fx :*: gx) = fmap k fx :*: fmap k gx

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldMap p (fa :*: ga) = foldMap p fa `mappend` foldMap p ga

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  traverse k (fx :*: gx) = (:*:) <$> traverse k fx <*> traverse k gx

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap k (L fx) = L (fmap k fx)
  fmap k (R gx) = R (fmap k gx)

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap p (L fx) = foldMap p fx
  foldMap p (R gx) = foldMap p gx

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse k (L fx) = L <$> traverse k fx
  traverse k (R gx) = R <$> traverse k gx

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap k (C fgx) = C (fmap (fmap k) fgx)

instance (Traversable f, Traversable g) => Traversable (f :.: g) where
  traverse k (C fgx) = C <$> traverse (traverse k) fgx

{---------------------------------------------------------------------}
-- triples of triples, and their transposes

type Triple = I :*: I :*: I

pattern Tr :: x -> x -> x -> (:*:) (I :*: I) I x
pattern Tr a b c = I a :*: I b :*: I c

type Zone = Triple :.: Triple

-- what's for free?

zone :: Zone Char
zone = C (Tr (Tr 'a' 'b' 'c') (Tr 'd' 'e' 'f') (Tr 'g' 'h' 'i'))

{---------------------------------------------------------------------}
-- Newtype piggery-jokery

class Newtype new where
  type Old new
  pack :: Old new -> new
  unpack :: new -> Old new

newly :: (Newtype a, Newtype b) => (Old a -> Old b) -> a -> b
newly f = pack . f . unpack

ala ::
  (Newtype b, Newtype d) =>
  ((a -> b) -> c -> d) ->
  (Old b -> b) ->
  (a -> Old b) ->
  c ->
  Old d
ala hof _ f = unpack . hof (pack . f)

infixl 9 `ala`

instance Newtype ((f :.: g) x) where
  type Old ((f :.: g) x) = f (g x)
  pack = C
  unpack = unC

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

{-----------------------------------------------------------------------}

class Show a => Display a where
  display :: a -> String

instance Show a => Display (Maybe a) where
  display Nothing = "."
  display (Just a) = show a

instance Display a => Display (I a) where
  display (I x) = display x

instance Display a => Display (K a x) where
  display (K a) = display a

instance (Display (f (g a))) => Display ((f :.: g) a) where
  display  = (<> "\n" ) . display . unC

instance  (Display (f a), Display (g a)) => Display ((f :*: g) a) where
  display  (f :*: g) = display f <> display g

{-----------------------------------------------------------------------}
-- sudoku boards

type Board = Zone :.: Zone

pboard :: P (Board Cell)
pboard = sequenceA (pure pcell)

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

-- transpose a board
xpBoard :: Board Cell -> Board Cell
xpBoard = newly sequenceA

-- turn each box into a board row
boxBoard :: Board Cell -> Board Cell
boxBoard = newly (fmap C . newly (fmap sequenceA) . fmap unC)

-- where is the program?
main :: IO ()
main = do
  --- adds three extra newlines...
  putStr $ display $ parseBoard tryThis 

parseBoard :: String -> Board Cell
parseBoard s = b
  where
    [(b, _)] = parse pboard s

crush :: (Traversable f, Monoid b) => (a -> b) -> f a -> b
crush f = unK . traverse (K . f)  -- foldMap

reduce :: (Traversable t, Monoid o) => t o -> o
reduce = fold -- foldMap id -- fold / cf: mconcat

flatten :: (Traversable f) => f a -> [a]
flatten = foldMap (: [])

duplicates :: (Traversable f, Eq a) => f a -> [a]
duplicates = undefined
