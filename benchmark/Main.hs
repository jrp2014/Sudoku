module Main
  ( main
  ) where

import           Criterion.Main
import           Data.Maybe
import qualified Hutton
import qualified Norvig
import qualified Sudoku
import qualified Types4
import qualified Generics

bms solver =
  [ bench "tryThis" $ nf solver Sudoku.tryThis
  , bench "easy" $ nf solver Sudoku.easy
  , bench "gentle" $ nf solver Sudoku.gentle
  , bench "diabolical" $ nf solver Sudoku.diabolical
  , bench "unsolvable" $ nf solver Sudoku.unsolvable
  , bench "minimal" $ nf solver Sudoku.minimal {-,
                                               bench "nefarious" $ nf solver Sudoku.nefarious -}
  ]

bmGroups :: [Benchmark]
bmGroups =
  [ bgroup "Sudoku.solve5" $ bms Sudoku.solve5
  , bgroup "Generics.solve5" $ bms Types4.solve5
  , bgroup "Types4.solve5" $ bms Types4.solve5
  , bgroup "Hutton.solve4" $ bms (Hutton.solve4 . lines)
  , bgroup "Norvig.solve4"
    $ bms (maybe "Failed to solve" Norvig.gridToString . Norvig.solve)
  ]

main :: IO ()
main = defaultMain bmGroups
