-- Test.hs: Testing
-- 2004-Jul-17 / TN

module Test where

  -- Verification:

  testVerify :: [(a,[b])] -> (Bool,[(a,[b])])
  testVerify checks
    = (all ((== 1) . length . snd) checks, checks)
