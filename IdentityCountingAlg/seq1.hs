import Data.List 
import Control.Parallel.Strategies
import System.Environment
import System.IO

a' :: Int -> Int -> Int
a' 0 n = [n+1, 2*n+5, 2*n-1, (n-3) `div` 2]!!(n `mod` 4)
a' 1 n = [n `div` 2, 2*n+1, n-1, 2*n+1]!!(n `mod` 4)
a' 2 n = [2*n+7,n,(n-2) `div` 2, 2*n-3]!!(n `mod` 4)
a' 3 n = [n `div` 2, 2*n+3, 2*n-3, n]!!(n `mod` 4)
a' 4 n = [n+3, 2*n-1, (n-2) `div` 2, 2*n-1]!!(n `mod` 4)
a' 5 n = [2*n+5, (n-1) `div` 2, n+1, 2*n-5]!!(n `mod` 4)

evalFunk a n [] 		= n
evalFunk a n (x:xs) 	= a x (evalFunk a n xs)


solve nTuple ts = let
		nUpperbound = 2^(((nTuple + 1) `div` 2) + 1)
	in
		[ xs | xs <- ts, and [evalFunk a' n xs == n | n <- [0..nUpperbound-1] ]]


{-
Vi laver en sekvens med
0,1,2 på første plads
0,1,2,3,4,5 på næste pladser
-}

ct2'' :: Bool -> [Int] -> [[Int]]
ct2'' useOffset [] = [[]]
ct2'' useOffset (o:os) = [i : xs | i <- [(if useOffset then o else 0)..5], xs <- (ct2'' (useOffset && i==o) os)]
ct2' useOffset (o:os) = [i : xs | i <- [(if useOffset then o else 0)..2], xs <- (ct2'' (useOffset && i==o) os)] -- sæt første element som offset ved de senere!
ct2 (o:os) = ct2' True (o:os)

nthStringBase' 0 b = []
nthStringBase' n b = n `mod` b : nthStringBase' (n `div` b) b
nthStringBase n b l = (take (l - (length xs)) (repeat 0)) ++ reverse xs
	where xs = nthStringBase' n b

firstDigitAndOffsets nTuple = scanl (\(j1,v1) (j2,v2) -> (j2, v1+v2)) (0,0) [(i ,6^(nTuple - 1)) | i<- [1..3] ]
firstDigitAndOffset nTuple n = last $ takeWhile (\(_,v) -> v <= n) $ firstDigitAndOffsets nTuple

nthString nTuple n = d : nthStringBase (n-o) 6 (nTuple-1)
	where (d,o) = firstDigitAndOffset nTuple n
{-
	Hv	
-}

problemSize nTuple = snd $ last $ firstDigitAndOffsets nTuple

tasks :: Int -> Int -> [([Int],Int)]
tasks n p = zip (map (nthString n) os) xs
	where
		total = problemSize n
		l = total `div` p
		o = total `mod` p
		xs = (take o $ repeat (l+1)) ++ (take (p-o) $ repeat l)
		os = scanl (+) 0 xs

-- hvert af problemerne er nu givet ved "nthString" 

runTask nTuple (offset, len) = solve nTuple (take len $ ct2 offset)


computeSolution nTuple nCores = res where
	ts = tasks nTuple nCores
	{- resultPlainOld = runSingleTask nTuple
	resultPlain = map (runTask nTuple) ts
	resultSeq = parMap rseq (runTask nTuple) ts -}
	resultPar = parMap rpar (runTask nTuple) ts
	res = map descSol $ concat resultPar

descSol xs = (xs,2)
descSolStr (a,_) = show a

{-
Nu gør vi brug af parallelisering: 
-}
main = do
		(nTupleStr:nCoresStr:_) <- getArgs
		let res = computeSolution nTuple nCores where 
			nTuple = (read nTupleStr :: Int)
			nCores = (read nCoresStr :: Int)
		mapM putStrLn $ map descSolStr res
		putStrLn $ "Total count: " ++ (show $ sum $ map snd res)