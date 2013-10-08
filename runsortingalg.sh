./src/newSortingAlg $1 > /tmp/tg$1
gsort --parallel 8 -S 12G /tmp/tg$1 > /tmp/tg$1_sorted
./src/getSolutionFromSortedFile < /tmp/tg$1_sorted +RTS -K1000M -RTS