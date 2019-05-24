-- Merge.hs: Merge functions
-- 2005-Oct-05 / TN
-- Test: t43.hs

module Merge where

  -- Basic merging:

  merMergeBy cmp s1 [] = s1
  merMergeBy cmp [] s2 = s2
  merMergeBy cmp s1@(e1:es1) s2@(e2:es2)
    = if cmp e1 e2 == GT then
        e2 : merMergeBy cmp s1 es2
      else
        e1 : merMergeBy cmp es1 s2

  merMerge s1 s2 = merMergeBy compare s1 s2

  -- Special merging:

  merCompareSub f a b = compare (f a) (f b)

  merMergeByF f = merMergeBy (merCompareSub f)
