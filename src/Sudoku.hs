{-# LANGUAGE LambdaCase #-}

module Sudoku where

import           Data.Char
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Data.Traversable



{---------------------------------------------------------------------}
-- the functor kit

newtype         I x = I x                 deriving Show
newtype       K a x = K a                 deriving Show
data    (f :*: g) x = f x :*: g x         deriving Show
data    (f :+: g) x = L (f x) | R (g x)   deriving Show
newtype (f :.: g) x = C {unC :: f (g x)}  deriving Show


instance Applicative I where
  pure = I
  I f <*> I s = I (f s)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  hiding instance Functor
  pure x = pure x :*: pure x
  (ff :*: gf) <*> (fs :*: gs) = (ff <*> fs) :*: (gf <*> gs)

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  hiding instance Functor
  pure x = C (|(|x|)|)
  C fgf <*> C fgs = C (|fgf <*> fgs|)

instance Monoid a => Applicative (K a) where
  hiding instance Functor
  pure x = K mempty
  K f <*> K s = K (mappend f s)

-- boring Functor and Traversable instances are elsewhere











instance Traversable (K a) where
  traverse f (K a) = (|(K a)|)
instance Traversable I where
  hiding instance Functor
  traverse f (I x) = (|I (f x)|)
instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap k (fx :*: gx) = fmap k fx :*: fmap k gx
instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  hiding instance Functor
  traverse k (fx :*: gx) = (|traverse k fx :*: traverse k gx|)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap k (L fx) = L (fmap k fx)
  fmap k (R gx) = R (fmap k gx)
instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  hiding instance Functor
  traverse k (L fx) = (|L (traverse k fx)|)
  traverse k (R gx) = (|R (traverse k gx)|)
instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap k (C fgx) = C (fmap (fmap k) fgx)
instance (Traversable f, Traversable g) => Traversable (f :.: g) where
  hiding instance Functor
  traverse k (C fgx) = (|C (traverse (traverse k) fgx)|)

{---------------------------------------------------------------------}
-- triples of triples, and their transposes

type Triple = I :*: I :*: I

pattern Tr a b c = I a :*: I b :*: I c

type Zone = Triple :.: Triple

czone :: Zone Char
czone = C (Tr (Tr 'a' 'b' 'c') (Tr 'd' 'e' 'f') (Tr 'g' 'h' 'i'))












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
  pack = C
  unpack = unC

instance Newtype (Const a x) where
  type Old (Const a x) = a
  pack = Const
  unpack = getConst

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
-- sudoku boards

type Board = Zone :.: Zone

pboard :: P (Board Cell)
pboard = sequenceA (pure pcell)

tryThis :: String
tryThis = unlines
  ["...23.6.."
  ,"1.......7"
  ,".4...518."
  ,"5.....9.."
  ,"..73.68.."
  ,"..4.....5"
  ,".867...5."
  ,"4.......9"
  ,"..3.62..."
  ]

xpBoard :: Board Cell -> Board Cell
xpBoard = newly sequenceA

boxBoard :: Board Cell -> Board Cell
boxBoard = newly (fmap C . newly (fmap sequenceA) . fmap unC)

