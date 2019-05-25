-- Merge.hs: Merge functions
-- 2005-Oct-05 / TN
-- Test: t43.hs

module Merge where

  -- Basic merging:

  merMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
  merMergeBy cmp s1 [] = s1
  merMergeBy cmp [] s2 = s2
  merMergeBy cmp s1@(e1:es1) s2@(e2:es2)
    = if cmp e1 e2 == GT then
        e2 : merMergeBy cmp s1 es2
      else
        e1 : merMergeBy cmp es1 s2

  merMerge :: Ord a => [a] -> [a] -> [a]
  merMerge s1 s2 = merMergeBy compare s1 s2

  -- Special merging:

  merCompareSub :: Ord a => (t -> a) -> t -> t -> Ordering
  merCompareSub f a b = compare (f a) (f b)

  merMergeByF :: Ord a => (t -> a) -> [t] -> [t] -> [t]
  merMergeByF f = merMergeBy (merCompareSub f)
