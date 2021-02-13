

# This prog seems to run slower threaded..

ghc -O2 SdkMSol2.hs    -Wall \
                       -threaded \
                       -rtsopts \
                       -with-rtsopts=-N \
                       -Wincomplete-uni-patterns \
                       -Wincomplete-record-updates \
                       -Wcompat \
                       -Widentities \
                       -Wredundant-constraints \
                       -fhide-s -Tource-paths \
                       -Wmissing-export-lists \
                       -Wpartial-fields

ghc -O2 T40.hs         -Wall \
                       -threaded \
                       -rtsopts \
                       -with-rtsopts=-N \
                       -Wincomplete-uni-patterns \
                       -Wincomplete-record-updates \
                       -Wcompat \
                       -Widentities \
                       -Wredundant-constraints \
                       -fhide-s -Tource-paths \
                       -Wmissing-export-lists \
                       -Wpartial-fields

ghc -O2 T44.hs         -Wall \
                       -threaded \
                       -rtsopts \
                       -with-rtsopts=-N \
                       -Wincomplete-uni-patterns \
                       -Wincomplete-record-updates \
                       -Wcompat \
                       -Widentities \
                       -Wredundant-constraints \
                       -fhide-s -Tource-paths \
                       -Wmissing-export-lists \
                       -Wpartial-fields

./SdkMSol2 +RTS -N -s -RTS tn1 Traditional 3 -#123456789 1-53---9- ---6----- ------271 82------- ---487--- ------53- 23------- --7-59--- --6---8-4


./SdkMSol2 +RTS -N -s -RTS diabolical Traditional 3 -#123456789  \
                          -9-7--86- \
                          -31--5-2- \
                          8-6------ \
                          --7-5---6 \
                          ---3-7--- \
                          5---1-7-- \
                          ------1-9 \
                          -2-6--35- \
                          -54--8-7-

# First unsolvable" (requires backtracking) example:
./SdkMSol2 +RTS -N -s -RTS unsolvable Traditional 3 -#123456789  \
                          1--9-7--3 \
                          -8-----7- \
                          --9---6-- \
                          --72-94-- \
                          41-----95 \
                          --85-43-- \
                          --3---7-- \
                          -5-----4- \
                          2--8-6--9

# Minimal sized grid (17 values) with a unique solution:
./SdkMSol2 +RTS -N -s -RTS minimal Traditional 3 -#123456789  \
                          -98------ \
                          ----7---- \
                          ----15--- \
                          1-------- \
                          ---2----9 \
                          ---9-6-82 \
                          -------3- \
                          5-1------ \
                          ---4---2-

./SdkMSol2 +RTS -N -s -RTS example Traditional 3 -#123456789  \
                          -5--6---1 \
                          --48---7- \
                          8------52 \
                          2---57-3- \
                          --------- \
                          -3-69---5 \
                          79------8 \
                          -1---65-- \
                          5---3--6- \

./SdkMSol2 +RTS -N -s -RTS nefarious Traditional 3 -#123456789  \
                          ----6--8- \
                          -2------- \
                          --1------ \
                          -7----1-2 \
                          5---3---- \
                          ------4-- \
                          --42-1--- \
                          3--7--6-- \
                          -------5-

./SdkMSol2 +RTS -N -s -RTS counterbrute Traditional 3 -#123456789  \
                          --------- \
                          -----3-85 \
                          --1-2---- \
                          ---5-7--- \
                          --4---1-- \
                          -9------- \
                          5------73 \
                          --2-1---- \
                          ----4---9


./SdkMSol2 +RTS  -s -RTS wikipedia Traditional 3 -#123456789  \
  53--7---- 6--195--- -98----6- 8---6---3 4--8-3--1 7---2---6 -6----28- ---419--5 ----8--79 
