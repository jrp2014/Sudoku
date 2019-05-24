-- SortByF.hs: Sort by function of list element
-- 2005-May-18 / TN
-- Test: none

module SortByF where

  import List

  -- Special sorting:

  sbfCompareSub f a b = compare (f a) (f b)

  sortByF f = sortBy (sbfCompareSub f)
