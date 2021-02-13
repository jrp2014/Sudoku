module Main (main) where

import Criterion.Main
import qualified Hutton
import qualified Sudoku

bms solver =
  [ bench "tryThis" $ nf solver Sudoku.tryThis,
    bench "easy" $ nf solver Sudoku.easy,
    bench "gentle" $ nf solver Sudoku.gentle,
    bench "diabolical" $ nf solver Sudoku.diabolical,
    bench "unsolvable" $ nf solver Sudoku.unsolvable,
    bench "minimal" $ nf solver Sudoku.minimal {-,
                                               bench "nefarious" $ nf solver Sudoku.nefarious -}
  ]

bmGroups :: [Benchmark]
bmGroups =
  [ bgroup "solve5" $ bms Sudoku.solve5,
    bgroup "solve4" $ bms (Hutton.solve4 . lines)
  ]

main :: IO ()
main = defaultMain bmGroups
