import Data.List 

d1,d2,d3 :: Int -> Int
d1 n = [n+3, n+1, n-2, n-2]!!(n `mod` 4)
d2 n = [n+1, n-1]!!(n `mod` 2)
d3 n = [n+2, n+2, n-1, n-3]!!(n `mod` 4)

c1,c2 :: Int -> Int
c1 n = [2*n+3, (n-1) `div` 2, 2*n+3, n-2]!!(n `mod` 4)
c2 n = [2*n+1, n+2, 2*n+1, (n-3) `div` 2]!!(n `mod` 4)

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

test nTuple = let
		nUpperbound
		 | nTuple `mod` 2 == 0 = 2^((nTuple `div` 2) + 1)
		 | nTuple `mod` 2 == 1 = 2^(((nTuple + 1) `div` 2) + 1)
		aUpperbound = 2^(nTuple + 1) - 1
		a k n 
		 | n == aUpperbound+1 	= aUpperbound
		 | otherwise 			= min (a' k n) (aUpperbound + 1)
	in
	--	map (a 3) [0..aUpperbound+1] 
	[ xs | xs <- ct nTuple, and [evalFunk a n xs == n | n <- [0..nUpperbound-1] ] ,  isMinimal xs] -- Vi tester til sidst om den er minimal, da dette er tungt

{-
main = do
	putStrLn ("test1: " ++ (show test1))
	putStrLn ("test2: " ++ (show test2))
-}

main = putStrLn $ show $ test 5

problemSize nTuple pCount = (sum [ i^(nTuple - 1) | i <- [1..6]]) `div` pCount

splitPoint nTuple pCount k  = head $ dropWhile (\(_,x)->x >= problemSize nTuple pCount) [(j, (6-k+1)^(nTuple-1-j)) | j <- [0..nTuple - 1]]

---

-- o:os er offset
ct2' :: Bool -> Int -> [Int] -> [[Int]]
ct2' useOffset x [] = [[]]
ct2' useOffset x (o:os) = [i : xs | i <- [(if useOffset then o else x)..5], xs <- (ct2' (useOffset && i==o) x os)]
ct2 (o:os) = ct2' True o (o:os)

nthStringBase' 0 b = []
nthStringBase' n b = n `mod` b : nthStringBase' (n `div` b) b
nthStringBase n b l = (take (l - (length xs)) (repeat 0)) ++ reverse xs
	where xs = nthStringBase' n b

firstDigitAndOffset nTuple n = last $ takeWhile (\(_,v) -> v <= n) $ scanl (\(j1,v1) (j2,v2) -> (j2, v1+v2)) (0,0) [(i ,(6-i+1)^(nTuple - 1)) | i<- [1..6] ]

nthString nTuple n = d : (map (+d) $ nthStringBase (n-o) (6-d) (nTuple-1))
	where (d,o) = firstDigitAndOffset nTuple n