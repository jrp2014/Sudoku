-- SortByF.hs: Sort by function of list element
-- 2005-May-18 / TN
-- Test: none

module SortByF where

  import Data.List

  -- Special sorting:

  sbfCompareSub :: Ord a => (t -> a) -> t -> t -> Ordering
  sbfCompareSub f a b = compare (f a) (f b)

  sortByF :: Ord a => (t -> a) -> [t] -> [t]
  sortByF f = sortBy (sbfCompareSub f)
