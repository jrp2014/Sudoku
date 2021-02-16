module Main
  ( main,
  )
where

import Criterion.Main
import Data.Maybe
import qualified Generics
import qualified Hutton
import qualified Norvig
import qualified Sudoku
import qualified Types4

bmGroups :: [Benchmark]
bmGroups =
  [ bgroup "tryThis" $ bms Sudoku.tryThis,
    bgroup "easy" $ bms Sudoku.easy,
    bgroup "gentle" $ bms Sudoku.gentle,
    bgroup "diabolical" $ bms Sudoku.diabolical,
    bgroup "unsolvable" $ bms Sudoku.unsolvable,
    bgroup "minimal" $ bms Sudoku.minimal {-,
                                               bgroup "nefarious" $ bms Sudoku.nefarious -}
  ]

bms :: String -> [Benchmark]
bms puzzle =
  [ bench "Sudoku.solve5" $ nf Sudoku.solve5 puzzle,
    bench "Generics.solve5" $ nf Types4.solve5 puzzle,
    bench "Types4.solve5" $ nf Types4.solve5 puzzle,
    bench "Hutton.solve4" $ nf (Hutton.solve4 . lines) puzzle,
    bench "Norvig.solve4" $ nf (maybe "Failed to solve" Norvig.gridToString . Norvig.solve) puzzle
  ]

main :: IO ()
main = defaultMain bmGroups
