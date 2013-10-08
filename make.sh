ghc -O3 --make oldSortingAlg.hs -fforce-recomp -rtsopts -threaded
ghc -O3 --make newSortingAlg.hs -fforce-recomp -rtsopts -threaded
ghc -O3 --make getSolutionFromSortedFile.hs -fforce-recomp -rtsopts -threaded