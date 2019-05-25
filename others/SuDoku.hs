-- SuDoku.hs: Su Doku solution
-- 2019-May-25 / TN: add type sigs and hlint
-- 2005-Aug-23 / TN
-- 2005-Oct-05 / TN: sdkSubSetsSizeOrdered (several)
-- 2005-Oct-07 / TN: sdkReduceByField
-- 2005-Oct-08 / TN: sdkReduceByElement
-- 2005-Oct-09 / TN: More sdkReduceByElement
-- 2005-Oct-23 / TN: sdkAssignments, sdkSensible, and
--                   sdkAllElementListSets (unfinished)
-- 2005-Oct-25 / TN: More sdkAllElementListSets
-- 2005-Oct-29 / TN: Split sdkReduceByField and sdkReduceByElement to
--                    investigate difference between field and
--                    element reduction
-- 2006-Jan-14 / TN: Parameterize over set of constraint sets
-- 2006-Mar-26 / TN: Remove closure by element experiments
-- 2006-Maj-31 / TN: Remove sdkBoardParameter and sdkSubSets
-- 2006-Maj-31 / TN: Explain some more
-- Test: t40.hs

module SuDoku where

  import Data.List
  import Data.Array
  import System.IO
  import System.CPUTime

  import Format
  import SortByF
  import Merge

  -- Traditional constraint sets:

  sdkConstraintSetsTraditional :: (Eq a, Num a, Enum a) =>
                                  a -> [[(a, a)]]
  sdkConstraintSetsTraditional n
    = nub ( rows ++ columns ++ squares )
      where
      sindexes = [0..n*n-1]
      rows = [ [ (x,y) | y <- sindexes ] | x <- sindexes ]
      columns = [ [ (x,y) | x<-sindexes ] | y<- sindexes ]
      indexes = [0..n-1]
      square i j = [ (n*i+x,n*j+y) | x<-indexes, y<-indexes ]
      squares = [ square x y | x<-indexes, y<-indexes ]

  -- Rebase a constraint set:

  sdkConstraintSetRebase :: (Num a, Num b) =>
                            a -> b -> [(a, b)] -> [(a, b)]
  sdkConstraintSetRebase row col cs
    = map (\(x,y) -> (row+x,col+y)) cs

  -- Clover constraint sets:

  sdkConstraintSetsClover :: (Ord a, Enum a, Num a) =>
                             a -> [[(a, a)]]
  sdkConstraintSetsClover n
    = let
        baseSets = sdkConstraintSetsTraditional n
        n3 = 2*(n*n-n)
        n4 = n*n-n
        upRightSets = map (sdkConstraintSetRebase 0 n3) baseSets
        downLeftSets = map (sdkConstraintSetRebase n3 0) baseSets
        downRightSets = map (sdkConstraintSetRebase n3 n3) baseSets
        middleSets = map (sdkConstraintSetRebase n4 n4) baseSets
        allSets
          = baseSets
            ++ upRightSets
            ++ downLeftSets
            ++ downRightSets
            ++ middleSets
      in
        nub (map sort allSets)

  -- Subsets ordered by size (basic and improved(?)):

  sdkSubSetsSizeOrdered :: [a] -> [[a]]
  sdkSubSetsSizeOrdered [] = [[]]
  sdkSubSetsSizeOrdered (e:es)
    = let
        ss = sdkSubSetsSizeOrdered es
      in
        merMergeByF genericLength ss (map (e:) ss)

  -- Reduce constraint set by field:

  sdkElements :: Eq a1 => [(a2, [a1])] -> [a1]
  sdkElements st = (nub . concatMap snd) st

  sdkListLengthLE :: [a1] -> [a2] -> Bool
  sdkListLengthLE [] _ = True
  sdkListLengthLE (_:_) [] = False
  sdkListLengthLE (_:es1) (_:es2) = sdkListLengthLE es1 es2

  sdkReducing :: Eq a => [(a2, [a])] -> Bool
  sdkReducing st = sdkListLengthLE (sdkElements st) st

  sdkRemove :: Eq a1 => [a1] -> (a2, [a1]) -> (a2, [a1])
  sdkRemove elms (x,es) = (x,es\\elms)

  sdkReductionsByField :: (Eq a2, Eq a1) =>
                          [(a2, [a1])] -> [([(a2, [a1])], [(a2, [a1])])]
  sdkReductionsByField st
    = let
        []:nonEmptySubSets = sdkSubSetsSizeOrdered st
        reducingSubSets = filter sdkReducing nonEmptySubSets
        reduceBy r = ( r , map (sdkRemove (sdkElements r)) (st \\ r) )
      in
        map reduceBy reducingSubSets

  sdkReduceByField :: (Eq a2, Eq a1) => [(a2, [a1])] -> [(a2, [a1])]
  sdkReduceByField st
    = let
        reds = sdkReductionsByField st
      in
        if null reds then
          st
        else
          let
            (firstReducer,rest) = head reds
          in
            firstReducer ++ sdkReduceByField rest

  -- Reduce constraint set by element:

  sdkUses :: Eq a1 => [a1] -> (a2, [a1]) -> Bool
  sdkUses es1 (_,es2) = intersect es1 es2 /= []

  sdkAttachFields :: Eq a1 =>
                         [(a2, [a1])] -> [a1] -> ([a1], [(a2, [a1])])
  sdkAttachFields st es = (es,filter (sdkUses es) st)

  sdkIsReducingByElement :: ([a1], [a2]) -> Bool
  sdkIsReducingByElement (es,sts) = genericLength es == genericLength sts

  sdkIntersect :: Eq a1 => [a1] -> (a2, [a1]) -> (a2, [a1])
  sdkIntersect elms (x,es) = (x,es`intersect`elms)

  sdkReductionsByElement :: (Eq a2, Eq a1) =>
                            [(a2, [a1])] -> [([(a2, [a1])], [(a2, [a1])])]
  sdkReductionsByElement st
    = let
        es = sdkElements st
        []:nonEmptyElementSubSets = sdkSubSetsSizeOrdered es
        reducingCandidates = map (sdkAttachFields st) nonEmptyElementSubSets
        reducingElements = filter sdkIsReducingByElement reducingCandidates
        reduceBy (redes,redsts)
          = ( map (sdkIntersect redes) redsts, st \\ redsts )
      in
        map reduceBy reducingElements

  sdkReduceByElement :: (Eq a2, Eq a1) =>
                        [(a2, [a1])] -> [(a2, [a1])]
  sdkReduceByElement st
    = let
        reds = sdkReductionsByElement st
      in
        if null reds then
          st
        else
          let
            (firstReducer,rest) = head reds
          in
            firstReducer ++ sdkReduceByElement rest

  -- Extract constraint set from board:

  sdkBoardExtract :: Ix i => Array i b -> [i] -> [(i, b)]
  sdkBoardExtract bd cs = map (\x -> ( x, bd!x )) cs

  -- Reduce board by single constraint set:

  sdkBoardReduce :: Ix i =>
                    ([(i, b)] -> [(i, b)]) ->
                    Array i b -> [i] -> Array i b
  sdkBoardReduce by bd cs = bd // by ( sdkBoardExtract bd cs )

  -- Reduce board by all constraint sets:

  sdkBoardReduceAll :: (Foldable t, Ix i) =>
                       ([(i, b)] -> [(i, b)]) -> t [i] ->
                       Array i b -> Array i b
  sdkBoardReduceAll by css bd
    = foldl (sdkBoardReduce by) bd css

  -- Board closure (reduce until no changes). Note that the entire list of
  -- intermediate boards are produced to allow inspection:

  sdkBoardClosure :: (Eq b, Foldable t, Ix i) =>
                     ([(i, b)] -> [(i, b)]) -> t [i] ->
                     Array i b -> [Array i b]

  sdkBoardClosure by css bd
    = bd : (
        let
          bd1 = sdkBoardReduceAll by css bd
        in
          if bd == bd1 then
            []
          else
            sdkBoardClosure by css bd1
      )

  -- Find non-singles on board:

  sdkBoardNonSingles :: (Foldable t, Ix a1) =>
                        Array a1 (t a2) -> [(a1, t a2)]
  sdkBoardNonSingles bd = filter ((/=1) . length . snd) (assocs bd)

  -- Board solve:

  sdkSolve :: (Eq a, Foldable t, Ix i) =>
              ([(i, [a])] -> [(i, [a])]) -> t [i] ->
              Array i [a] -> [Array i [a]]
  sdkSolve by css bd
    = let
        bdc = last (sdkBoardClosure by css bd)
        nss = sdkBoardNonSingles bdc
      in
        if null nss then
          [bdc]
        else
          let
            (ix,ps) = head (sortByF (length . snd) nss)
          in
            concatMap
                  (\p -> sdkSolve by css ( bdc // [(ix,[p])] ))
                  ps

  -- Show board (assuming that elements are single characters):

  sdkBoardShow :: (Enum a, Num a, Ix a, Ix b, Num b, Enum b) =>
                  Array (a, b) String -> String
  sdkBoardShow bd
    = let
        ((0,0),(n,m)) = bounds bd
        w = maximum (map length (elems bd))
        pad s = replicate (w - length s) ' ' ++ s
        showRow i = unwords (map (\ j -> pad (bd ! (i, j))) [0 .. m])
      in
        concat [ showRow i ++ "\n"  | i<-[0..n] ]

  sdkBoardListShow :: (Ix a, Ix b, Num a, Num b, Enum a, Enum b) =>
                      [Array (a, b) String] -> String
  sdkBoardListShow bds =
    concatMap ((++"\n") . forIndent2 2 . sdkBoardShow) bds

  -- Assignments:

  sdkAssignments :: Eq a => [[a]] -> [[a]]
  sdkAssignments [] = [[]]
  sdkAssignments (es:ess)
    = concatMap (\e -> map (e:) (sdkAssignments (map (delete e) ess))) es

  -- A list of element sets is sensible if assignable and no superfluous elements:

  sdkSensible :: Eq a => [[a]] -> Bool
  sdkSensible ess = length ess == length ((nub . concat) ess) && sdkAssignments ess /= []

  -- All lists of element sets using given symbols:

  sdkAllElementListSets :: [a] -> [[[a]]]
  sdkAllElementListSets es = sdkSizedElementListSets (genericLength es) (sdkSubSetsSizeOrdered es)

  sdkSizedElementListSets :: (Eq t, Num t) => t -> [a] -> [[a]]
  sdkSizedElementListSets 0 _ = [[]]
  sdkSizedElementListSets _ [] = []
  sdkSizedElementListSets n ess@(es:ess')
    = map (es:) (sdkSizedElementListSets (n-1) ess)
        ++ sdkSizedElementListSets n ess'

  -- Reduce by field and element is the same:

  sdkReductionsIdentical :: Ord a1 => [[a1]] -> Bool
  sdkReductionsIdentical ess
    = let
        st = map ((,) ()) ess
      in
        sort (sdkReduceByField st) == sort (sdkReduceByElement st)

  -- Amended reductions:

  sdkReductionsAmended :: (Ord a1, Ord a2) =>
                          ([a1] -> [([a2], [a2])]) ->
                          [a1] -> [([a1], [a2])] 
  sdkReductionsAmended by st = map (\(r,rst) -> (sort st, sort (r++rst))) (by st)

  sdkReductions :: (Ord a1, Ord a2, Num a1, Enum a1) =>
                   ([(a1, [a2])] -> [([(a1, [a2])], [(a1, [a2])])])
                   -> [a2] -> [([(a1, [a2])], [(a1, [a2])])]
  sdkReductions by es
    = filter
        (uncurry (/=)) $
        concatMap (sdkReductionsAmended by . zip [0 ..])
                  (filter sdkSensible (sdkAllElementListSets es))

  -- Processing:

  sdkProcessSuDoku :: (Num a, Num b, Enum a, Enum b, Foldable t,
                      Ix a, Ix b) =>
                      String -> t [(a, b)] -> Array (a, b) String -> IO ()
  sdkProcessSuDoku process_name css bd
    = do
        cpuStart <- getCPUTime
        putStr ( process_name ++ ": Problem:"
          ++ "\n" ++ forIndent2 2 ( sdkBoardShow bd )
          ++ "\n" )
        hFlush stdout
        putStr ( process_name
          ++ ": Closure by field:\n"
          ++ sdkBoardListShow
               ( sdkBoardClosure sdkReduceByField css bd )
          ++ "\n" )
        -- putStr ( process_name
          -- ++ ": Closure by element:\n"
          -- ++ sdkBoardListShow
               -- ( sdkBoardClosure sdkReduceByElement css bd )
          -- ++ "\n" )
        -- putStr ( process_name
          -- ++ ": Closure by both field and element:\n"
          -- ++ sdkBoardListShow
               -- ( sdkBoardClosure
                   -- (sdkReduceByField . sdkReduceByElement)
                   -- css
                   -- bd
               -- )
          -- ++ "\n" )
        let
          bds = sdkSolve sdkReduceByField css bd
          in
            do
              putStr ( process_name ++ ": Solutions:\n"
                ++ sdkBoardListShow bds
                ++ "\n" )
              putStr ( process_name ++ ": Total of " ++ show (length bds) ++ " solutions\n" )
              cpuEnd <- getCPUTime
              putStr ( process_name ++ ": CPU Used: " ++
                show ( (fromIntegral (cpuEnd - cpuStart)::Double)/10^12 ) ++ "\n" )
