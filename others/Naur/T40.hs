-- t40.hs: SuDoku tests
-- 2005-Aug-23 / TN
-- 2005-Oct-05 / TN: Faster (?) subsets sorted by size
-- 2005-Oct-07 / TN: ReduceByField
-- 2005-Oct-08 / TN: ReduceByElement
-- 2005-Oct-23 / TN: Checking equivalence of ReduceByField and
--                   ReduceByElement
-- 2006-Jan-14 / TN: Parameterize over constraint sets
-- 2006-May-31 / TN: Add sdkSubSets

module Main where

  import Data.List
  import Data.Array

  import Test
  import Format
  import SortByF

  import SuDoku

  -- Verify all:

  t40Do :: String -> String -> String
  t40Do s r
    = "t40: sdkVerify" ++ s ++ ": " ++ forIndent1 2 r ++ "\n"

  main :: IO ()
  main
    = do
        putStr "t40: 2005-Aug-24 20.47\n"
        putStr (t40Do "A" (show sdkVerifyA))
        putStr (t40Do "B" (show sdkVerifyB))
        putStr (t40Do "C" (show sdkVerifyC))
        putStr (t40Do "D" (show sdkVerifyD))
        putStr (t40Do "E" (show sdkVerifyE))
        putStr (t40Do "F" (show sdkVerifyF))
        putStr (t40Do "G" (show sdkVerifyG))
        putStr (t40Do "H" (show sdkVerifyH))
        putStr (t40Do "I" (show sdkVerifyI))

  -- Subsets:

  sdkSubSets :: [a] -> [[a]]
  sdkSubSets [] = [[]]
  sdkSubSets (e:es)
    = let
        ss = sdkSubSets es
      in
        ss ++ map (e:) ss

  -- Test board:

  sdkVerifyBoard :: Array (Int, Int) String
  sdkVerifyBoard
    = let
        n = 2
        n2 = n*n
        --all = concatMap show [1..n2]
      in
        listArray ((0,0),(n2-1,n2-1)) (map (show . (+1) . (`mod`9)) [0..])

  -- SuDoku constraint set test:

  sdkVerifyACheck1 :: (Eq a, Num a, Enum a) =>
                      a -> [[(a, a)]] -> (a, [[[(a, a)]]])
  sdkVerifyACheck1 n sdkCheck
    = ( n, nub [sdkConstraintSetsTraditional n, sdkCheck] )

  sdkVerifyA :: (Bool, [(Int, [[[(Int, Int)]]])])
  sdkVerifyA
    = testVerify
        [
          sdkVerifyACheck1 1 [[(0,0)]]
          , sdkVerifyACheck1 2 [
            [(0,0),(0,1),(0,2),(0,3)]
            , [(1,0),(1,1),(1,2),(1,3)]
            , [(2,0),(2,1),(2,2),(2,3)]
            , [(3,0),(3,1),(3,2),(3,3)]
            , [(0,0),(1,0),(2,0),(3,0)]
            , [(0,1),(1,1),(2,1),(3,1)]
            , [(0,2),(1,2),(2,2),(3,2)]
            , [(0,3),(1,3),(2,3),(3,3)]
            , [(0,0),(0,1),(1,0),(1,1)]
            , [(0,2),(0,3),(1,2),(1,3)]
            , [(2,0),(2,1),(3,0),(3,1)]
            , [(2,2),(2,3),(3,2),(3,3)]
            ]
        ]


  -- SuDoku subset test:

  sdkVerifyBCheck1 :: Eq a => [a] -> [[a]] -> ([a], [[[a]]])
  sdkVerifyBCheck1 st sdkCheck
    = ( st, nub  [sdkSubSets st, sdkCheck] )

  sdkVerifyB :: (Bool, [(String, [[String]])])
  sdkVerifyB
    = testVerify
        [
          sdkVerifyBCheck1 "ABC" ["","C","B","BC","A","AC","AB","ABC"]
          , sdkVerifyBCheck1 "" [""]
        ]


  -- SuDoku reduce constraint set by field test:

  sdkVerifyCCheck1 :: (Eq a2, Eq a1) =>
                      [(a2, [a1])] -> [(a2, [a1])] -> ([(a2, [a1])], [[(a2, [a1])]])
  sdkVerifyCCheck1 st sdkCheck
    = ( st, nub [sdkReduceByField st, sdkCheck] )

  sdkVerifyC :: (Bool, [([(Int, String)], [[(Int, String)]])])
  sdkVerifyC
    = testVerify
        [
          sdkVerifyCCheck1
            [(1,"3"),(7,"2"),(4,"1234567890")]
            [(7,"2"),(1,"3"),(4,"14567890")]
          , sdkVerifyCCheck1
              [(2,"79"),(8,"97"),(5,"1234567890")]
              [(2,"79"),(8,"97"),(5,"12345680")]
          , sdkVerifyCCheck1
              [(3,"79"),(9,"93"),(13,"37"),(7,"1234567890")]
              [(3,"79"),(9,"93"),(13,"37"),(7,"1245680")]
          , sdkVerifyCCheck1
              [(23,"3"),(4,"79"),(10,"93"),(14,"37"),(8,"1234567890")]
              [(23,"3"),(14,"7"),(10,"9"),(4,""),(8,"1245680")]
        ]

  -- Board extraction test:

  sdkVerifyDCheck1 :: (Eq b, Ix i) =>
                      Array i b -> [i] -> [(i, b)] -> ((Array i b, [i]), [[(i, b)]])
  sdkVerifyDCheck1 bd cs sdkCheck
    = ( ( bd, cs), nub [sdkBoardExtract bd cs, sdkCheck] )

  sdkVerifyD :: (Bool, [((Array (Int, Int) String, [(Int, Int)]),
                       [[((Int, Int), String)]])])
  sdkVerifyD
    = testVerify
        [
          sdkVerifyDCheck1
            sdkVerifyBoard
            [(0,0),(2,2)]
            [((0,0),"1"),((2,2),"2")]
        ]

  -- Board reduction test:

  sdkVerifyECheck1 :: (Ix i, Eq a1) =>
                      Array i [a1]
                      -> [i] -> Array i [a1] -> ((Array i [a1], [i]), [Array i [a1]])
  sdkVerifyECheck1 bd cs sdkCheck
    = ( ( bd, cs), nub [sdkBoardReduce sdkReduceByField bd cs, sdkCheck] )

  sdkVerifyE :: (Bool, [((Array (Int, Int) String, [(Int, Int)]),
                       [Array (Int, Int) String])])
  sdkVerifyE
    = testVerify
        [
          sdkVerifyECheck1
            sdkVerifyBoard
            [(0,0),(2,1)]
            ( sdkVerifyBoard // [((0,0),"")] )
        ]

  -- SuDoku size-sorted subset test:

  sdkVerifyFCheck1 :: Eq a => [a] -> ([a], [[[a]]])
  sdkVerifyFCheck1 st
    = (
        st,
        nub
          [sdkSubSetsSizeOrdered st, (sortByF genericLength . sdkSubSets) st]
      )

  sdkVerifyF :: (Bool, [(String, [[String]])])
  sdkVerifyF
    = testVerify
        [
          sdkVerifyFCheck1 "ABC"
          , sdkVerifyFCheck1 ""
        ]

  -- SuDoku reduce constraint set by element test:

  sdkVerifyGCheck1 :: (Eq a2, Eq a1) =>
                      [(a2, [a1])] -> [(a2, [a1])] -> ([(a2, [a1])], [[(a2, [a1])]])
  sdkVerifyGCheck1 st sdkCheck
    = ( st, nub [sdkReduceByElement st, sdkCheck] )

  sdkVerifyG :: (Bool, [([(Int, String)], [[(Int, String)]])])
  sdkVerifyG
    = testVerify
        [
          sdkVerifyGCheck1
            [(1,"13"),(2,"139"),(3,"459"),(4,"4"),(5,"5")]
            [(1,"13"),(2,"13"),(3,"9"),(5,"5"),(4,"4")]
        ]

  -- SuDoku compare reduce by field and by element:

  sdkVerifyHCheck1 :: (Ord a2, Ord a1) =>
                      [(a2, [a1])] -> ([(a2, [a1])], [[(a2, [a1])]])
  sdkVerifyHCheck1 st
    = ( st, (nub . map sort) [sdkReduceByElement st, sdkReduceByField st] )

  sdkVerifyH :: (Bool, [([(Int, String)], [[(Int, String)]])])
  sdkVerifyH
    = let
        all = "123456789"
      in
        testVerify
          [
            sdkVerifyHCheck1
              [(1,"13"),(2,"139"),(3,"459"),(4,"4"),(5,"5")]
            , sdkVerifyHCheck1
                [(1,all),(2,"1"),(3,"4"),(4,all),(5,"5"),(6,all),(7,all),(8,all),(9,all)]
          ]

  -- SuDoku assignments and sensible test:

  sdkVerifyICheck1 :: Eq a => [[a]] -> (Bool, [[a]]) -> ([[a]], [(Bool, [[a]])])
  sdkVerifyICheck1 es sdkCheck
    = ( es, nub [(sdkSensible es,sdkAssignments es), sdkCheck] )

  sdkVerifyI :: (Bool, [([String], [(Bool, [String])])])
  sdkVerifyI
    = testVerify
        [
          sdkVerifyICheck1 ["13","2","3"] (True,["123"])
          , sdkVerifyICheck1 ["123","23","23"] (True,["123","132"])
          , sdkVerifyICheck1 ["1234","23","23"] (False,["123","132","423","432"])
          , sdkVerifyICheck1 ["123","123",""] (False,[])
        ]
