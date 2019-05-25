-- Format.hs: Formatting
-- 2004-Oct-14 / TN
-- 2005-Feb-20 16.57 / TN: Indent all

module Format where

  import Data.List

  -- Indenting subsequent lines:

  forIndent1 :: Int -> String -> String
  forIndent1 n = unlines . (\(l:ls)-> l:map (replicate n ' ' ++) ls) . lines

  -- Indenting all lines:

  forIndent2 :: Int -> String -> String
  forIndent2 n = unlines . map (replicate n ' ' ++) . lines

  -- Show a list:

  forListShow :: Show a => [a] -> String
  forListShow l
    = "["
        ++ intercalate "\n  , " (map show l)
        ++ "\n]"

  -- Show an Integer list summary:

  forIntegerListGroupFirst :: (Eq a, Num a) => [a] -> (a, a, [a])
  forIntegerListGroupFirst (x:xs)
    = groupFirst x x xs
      where
      groupFirst xlow xhigh [] = (xlow,xhigh,[])
      groupFirst xlow xhigh xs@(x:xs')
        = if x == xhigh + 1 then
            groupFirst xlow x xs'
          else
            (xlow,xhigh,xs)

  forIntegerListGroup :: (Eq b, Num b) => [b] -> [(b, b)]
  forIntegerListGroup [] = []
  forIntegerListGroup xs@(_:_)
    = let
        (x1,x2,rest) = forIntegerListGroupFirst xs
      in
        (x1,x2) : forIntegerListGroup rest

  forIntegerGroupShow :: (Show a, Enum a) => (a, a) -> String
  forIntegerGroupShow (x1,x2)
    = let
        show1 = intercalate "," (map show [x1..x2])
        show2 = show x1 ++ ".." ++ show x2
      in
        if length show1 <= length show2 then
          show1
        else
          show2

  forIntegerListShow1 :: (Show a, Enum a, Num a, Ord a) =>
                             [a] -> String
  forIntegerListShow1 l
    = intercalate
        ","
        (map forIntegerGroupShow (forIntegerListGroup (sort l)))
