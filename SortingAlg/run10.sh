./ThompsonGroupOdd 10 > /Users/soren/Documents/tgo10
gsort --parallel 2 -S 2G /Users/soren/Documents/tgo10 > /Users/soren/Documents/tgo10_sorted
./getSolutionFromSortedFile < /Users/soren/Documents/tgo10_sorted +RTS -K1000M -RTS
