-- Format.hs: Formatting
-- 2004-Oct-14 / TN
-- 2005-Feb-20 16.57 / TN: Indent all

module Format where

  import List

  -- Indenting subsequent lines:

  forIndent1 n = unlines . (\(l:ls)-> l:map (replicate n ' ' ++) ls) . lines

  -- Indenting all lines:

  forIndent2 n = unlines . map (replicate n ' ' ++) . lines

  -- Show a list:

  forListShow l
    = "["
        ++ concat (intersperse "\n  , " (map show l))
        ++ "\n]"

  -- Show an Integer list summary:

  forIntegerListGroupFirst (x:xs)
    = groupFirst x x xs
      where
      groupFirst xlow xhigh [] = (xlow,xhigh,[])
      groupFirst xlow xhigh xs@(x:xs')
        = if x == xhigh + 1 then
            groupFirst xlow x xs'
          else
            (xlow,xhigh,xs)

  forIntegerListGroup [] = []
  forIntegerListGroup xs@(x:xs')
    = let
        (x1,x2,rest) = forIntegerListGroupFirst xs
      in
        (x1,x2) : forIntegerListGroup rest

  forIntegerGroupShow (x1,x2)
    = let
        show1 = concat (intersperse "," (map show [x1..x2]))
        show2 = show x1 ++ ".." ++ show x2
      in
        if length show1 <= length show2 then
          show1
        else
          show2

  forIntegerListShow1 l
    = concat (
        intersperse
        ","
        (map forIntegerGroupShow (forIntegerListGroup (sort l)))
      )
