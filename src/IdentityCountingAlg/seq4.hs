import Data.List 
import Control.Parallel.Strategies
import System.Environment
import System.IO

b 0 n = [2*n, (n+3) `div`2, 2*n, n, 2*n, (n-3) `div` 2, 2*n, n]!!(n `mod` 8)
b 1 n = [n `div` 2, 2*n+1, n-1, 2*n+1]!!(n `mod` 4)
b 2 n = [n, 2*n+4, 2*n-2, (n-1) `div` 2]!!(n `mod` 4)
b 3 n = [n `div` 2, 2*n+3, 2*n-3, n]!!(n `mod` 4)
b 4 n = [2*n, n+1, 2*n, (n-1) `div` 2]!!(n `mod` 4)
b 5 n = [n, 2*n+1, (n+2) `div` 2, 2*n+1, n, 2*n+1,(n-4) `div` 2, 2*n+1]!!(n `mod` 8)

phi i = [1,-1,0,-1,1,0]!!i
psi i = [0,1,-1,0,-1,1]!!i

rots xs = init (zipWith (++) (tails xs) (inits xs))
isMinimal xs = and [xs <= ys | ys <- rots xs]

evalFunk a n [] 		= n
evalFunk a n (x:xs) 	= a x (evalFunk a n xs)


solve nTuple ts = let
		nUpperbound = 2^(((nTuple + 1) `div` 2) + 1)
	in
		[ xs | xs <- ts, sum (map phi xs) == 0, sum (map psi xs) == 0,  and [evalFunk b n xs == n | n <- [0..nUpperbound-1] ], isMinimal xs]



ct2'' :: Bool -> Int -> Int -> [Int] -> [[Int]]
ct2'' useOffset x p [] = [[]]
ct2'' useOffset x p (o:os) = [i : xs | i <- filter (>= (if useOffset then o else x)) $ f' p, xs <- (ct2'' (useOffset && i==o) x i os)]
ct2' useOffset x (o:os) = [i : xs | i <- [(if useOffset then o else x)..5], xs <- (ct2'' (useOffset && i==o) i i os)] -- sæt første element som offset ved de senere!
ct2 (o:os) = filter satisfiesRelation $ ct2' True o (o:os)

nthStringBase' 0 b = []
nthStringBase' n b = n `mod` b : nthStringBase' (n `div` b) b
nthStringBase n b l = (take (l - (length xs)) (repeat 0)) ++ reverse xs
	where xs = nthStringBase' n b

firstDigitAndOffsets nTuple = scanl (\(j1,v1) (j2,v2) -> (j2, v1+v2)) (0,0) [(i ,4^(nTuple - 1)) | i<- [1..3] ]
firstDigitAndOffset nTuple n = last $ takeWhile (\(_,v) -> v <= n) $ firstDigitAndOffsets nTuple

nthString nTuple n = d : nthStringBase (n-o) 4 (nTuple-1)
	where (d,o) = firstDigitAndOffset nTuple n
{-
TODO:
-}

-- x er tal fra 
f' x = [[0,2,4,5],[0,1,3,5],[1,2,3,4],[1,2,3,4],[0,2,4,5],[0,1,3,5]]!!x
f x y = (f' x)!!y
converttuple' x (y:ys) = z : converttuple' z ys where z = f x y
converttuple' _ [] = []
converttuple (x:xs) = x:converttuple' x xs


problemSize nTuple = snd $ last $ firstDigitAndOffsets nTuple

tasks :: Int -> Int -> [(Int,Int)]
tasks n p = zip os xs
	where
		total = problemSize n
		l = total `div` p
		o = total `mod` p
		xs = (take o $ repeat (l+1)) ++ (take (p-o) $ repeat l)
		os = scanl (+) 0 xs

-- hvert af problemerne er nu givet ved "nthString" 

runTask nTuple (offset, len) = solve nTuple (takeWhile (< maxLimit) $ ct2 offsetTuple)
	where 
		offsetTuple = converttuple $ nthString nTuple offset
		maxLimit = converttuple (nthString nTuple (offset + len))

computeSolution nTuple nCores = res where
	ts = tasks nTuple nCores
	{- resultPlainOld = runSingleTask nTuple
	resultPlain = map (runTask nTuple) ts
	resultSeq = parMap rseq (runTask nTuple) ts -}
	resultPar = parMap rpar (runTask nTuple) ts
	res = map descSol $ concat resultPar

descSol xs = (xs, 1+(length $ takeWhile (/= xs) $ tail $ rots xs))
descSolStr (a,b) = (show a) ++ " (orbit size: " ++ (show b) ++ ")" 


satisfiesRelation :: [Int] -> Bool
satisfiesRelation xs = and $ zipWith elem ((tail xs) ++ [head xs]) (map f' xs)

{-
Nu gør vi brug af parallelisering: 
-}
main = do
		(nTupleStr:nCoresStr:_) <- getArgs
		let nTuple = (read nTupleStr :: Int)
		let res = computeSolution nTuple nCores where 
			nCores = (read nCoresStr :: Int)
		--mapM putStrLn $ map descSolStr res
		putStrLn $ "Cyclic:"
		mapM putStrLn $ map descSolStr res
		putStrLn $ "Total count: " ++ (show $ sum $ map snd res)