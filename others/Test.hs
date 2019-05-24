-- Test.hs: Testing
-- 2004-Jul-17 / TN

module Test where

  -- Verification:

  testVerify :: [(a,[b])] -> (Bool,[(a,[b])])
  testVerify checks
    = (and (map ((==1) . length . snd) checks), checks)
