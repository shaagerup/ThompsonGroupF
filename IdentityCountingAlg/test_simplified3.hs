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

test1 = map (a1.a1.a1.a1.a1) [1..10] where a1 = a' 0
test2 = map (a6.a6.a6.a6.a6) [1..10] where a6 = a' 5

-- Candidate tuples
ct' nTuple x 0 = [[]]
ct' nTuple x n = [ i:xs | i <- [x..5], xs <- ct' nTuple x (n-1) ] 
ct nTuple = [ i:xs | i <- [0..5], xs <- ct' nTuple i (nTuple-1)]


rots xs = init (zipWith (++) (tails xs) (inits xs))
isMinimal xs = and [xs <= ys | ys <- rots xs]

evalFunk a n [] 		= n
evalFunk a n (x:xs) 	= a x (evalFunk a n xs)

evalFunk' a n aUpperbound [] = n
evalFunk' a n aUpperbound (x:xs)
 | res >= aUpperbound = aUpperbound
 | otherwise = a x res
	where res = evalFunk' a n aUpperbound xs
{- 
ts er en liste af tupler. Vi udnytter at Haskell er lazy her!
-}

solve nTuple ts = let
		nUpperbound = 2^(((nTuple + 1) `div` 2) + 1)
		aUpperbound = 2^(nTuple + 1)
	in
		[ xs | xs <- ts, and [evalFunk' a' n aUpperbound xs == n | n <- [0..nUpperbound-1] ] ,  isMinimal xs] -- Vi tester til sidst om den er minimal, da dette er tungt

runSingleTask nTuple = solve nTuple (ct nTuple)


{-
main = do
	putStrLn ("test1: " ++ (show test1))
	putStrLn ("test2: " ++ (show test2))
-}

--main = putStrLn $ show $ test 5


---

ct2'' :: Bool -> Int -> [Int] -> [[Int]]
ct2'' useOffset x [] = [[]]
ct2'' useOffset x (o:os) = [i : xs | i <- [(if useOffset then o else x)..5], xs <- (ct2'' (useOffset && i==o) x os)]
ct2' useOffset x (o:os) = [i : xs | i <- [(if useOffset then o else x)..5], xs <- (ct2'' (useOffset && i==o) i os)] -- sæt første element som offset ved de senere!
ct2 (o:os) = ct2' True o (o:os)

nthStringBase' 0 b = []
nthStringBase' n b = n `mod` b : nthStringBase' (n `div` b) b
nthStringBase n b l = (take (l - (length xs)) (repeat 0)) ++ reverse xs
	where xs = nthStringBase' n b

firstDigitAndOffsets nTuple = scanl (\(j1,v1) (j2,v2) -> (j2, v1+v2)) (0,0) [(i ,(6-i+1)^(nTuple - 1)) | i<- [1..6] ]
firstDigitAndOffset nTuple n = last $ takeWhile (\(_,v) -> v <= n) $ firstDigitAndOffsets nTuple

nthString nTuple n = d : (map (+d) $ nthStringBase (n-o) (6-d) (nTuple-1))
	where (d,o) = firstDigitAndOffset nTuple n

---

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

descSol xs = (xs, 1+(length $ takeWhile (/= xs) $ tail $ rots xs))
descSolStr (a,b) = (show a) ++ " (orbit size: " ++ (show b) ++ ")" 


computeSolution nTuple nCores = res where
	ts = tasks nTuple nCores
	{- resultPlainOld = runSingleTask nTuple
	resultPlain = map (runTask nTuple) ts
	resultSeq = parMap rseq (runTask nTuple) ts -}
	resultPar = parMap rpar (runTask nTuple) ts
	res = map descSol $ concat resultPar

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