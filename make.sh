ghc -O3 --make src/oldSortingAlg.hs -isrc -fforce-recomp -rtsopts -threaded
ghc -O3 --make src/newSortingAlg.hs -isrc -fforce-recomp -rtsopts -threaded
ghc -O3 --make src/newSortingAlgPar.hs -isrc -fforce-recomp -rtsopts -threaded
ghc -O3 --make src/getSolutionFromSortedFile.hs -isrc -fforce-recomp -rtsopts -threaded