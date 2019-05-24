-- SdkMSol2.hs: Su Doku Solve 2
-- 2006-Jan-14 / TN
-- 2006-Mar-26 16.17 / TN: Debugging (dos format shell script CR problem)
-- 2006-Jun-04 13.35 / TN: Definition decoding error

module Main where

  import System
  import IO
  import CPUTime
  import Array
  import List

  import Format

  import SuDoku

  -- Constraint set selection:

  sdkConstraintSetsSelect ("Traditional":n:as)
    = ( sdkConstraintSetsTraditional (read n), as )

  sdkConstraintSetsSelect ("Clover":n:as)
    = ( sdkConstraintSetsClover (read n), as )

  -- Set symbol definitions:

  sdkDefiner = '#'

  sdkSymbolDefinitions a
    = let
        isDef s = genericLength s >= 2 && s!!1 == sdkDefiner
        ds = filter isDef a
        a' = filter (not . isDef) a
        decode (s:d:v) | d == sdkDefiner = (s,v)
      in
        (map decode ds,a')

  -- Expansion:

  sdkSymbolExpand sd s
    = let
        xp = map snd (filter ((==s) . fst) sd)
      in
        if xp == [] then
          [s]
        else
          concat xp

  sdkRowExpand sd = map (sdkSymbolExpand sd)

  -- Row amendment:

  sdkRowAmend rowNo row = zip ([(rowNo,col) | col<-[0..]]) row

  -- Main program:

  progName = "SdkMSol2"

  main
    = do
        putStr ( progName ++ ": 2006-Jun-04 13.37\n" )
        args<-getArgs
        main0 args

  main0 args0
    = let
        name:args1 = args0
        (css,args2) = sdkConstraintSetsSelect args1
        (sd,rowsBase) = sdkSymbolDefinitions args2
        height = genericLength rowsBase
        width = maximum ( (map genericLength) rowsBase )
        rowsExpanded = map (sdkRowExpand sd) rowsBase
        rows
          = map
              (\(rowNo,row) -> sdkRowAmend rowNo row)
              (zip [0..] rowsExpanded)
        bd
          = array
              ((0,0),(height-1,width-1))
              ( concat rows )
        in
          do
            putStr ( progName ++ "(" ++ name ++ ")" ++ "\n" )
            putStr ( "  Height " ++ show height )
            putStr ( " Width " ++ show width ++ "\n")
            putStr
              (
                (forIndent2 2( concat (intersperse "\n" rowsBase) ) )
                ++ "\n"
              )
            sdkProcessSuDoku
              name
              css
              bd
