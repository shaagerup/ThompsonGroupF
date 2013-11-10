module ThompsonParallelSimple where

firstCipherBound :: Int
firstCipherBound = 3

otherCiphersBound :: Int
otherCiphersBound = 2

ct2'' :: Bool -> [Int] -> [[Int]]
ct2'' useOffset [] = [[]]
ct2'' useOffset (o:os) = [i : xs | i <- [(if useOffset then o else 0)..otherCiphersBound], xs <- (ct2'' (useOffset && i==o) os)]
ct2' useOffset (o:os) = [i : xs | i <- [(if useOffset then o else 0)..firstCipherBound], xs <- (ct2'' (useOffset && i==o) os)] -- sæt første element som offset ved de senere!
ct2 (o:os) = ct2' True (o:os)

nthStringBase' :: Int -> Int -> [Int]
nthStringBase' 0 b = []
nthStringBase' n b = n `mod` b : nthStringBase' (n `div` b) b

nthStringBase :: Int -> Int -> Int -> [Int]
nthStringBase n b l = (take (l - (length xs)) (repeat 0)) ++ reverse xs
	where xs = nthStringBase' n b

firstDigitAndOffsets :: Int -> [(Int,Int)]
firstDigitAndOffsets nTuple = scanl (\(j1,v1) (j2,v2) -> (j2, v1+v2)) (0,0) [(i ,(otherCiphersBound+1)^(nTuple - 1)) | i<- [1..firstCipherBound + 1] ]

firstDigitAndOffset :: Int -> Int -> (Int,Int)
firstDigitAndOffset nTuple n = last $ takeWhile (\(_,v) -> v <= n) $ firstDigitAndOffsets nTuple

nthString :: Int -> Int -> [Int]
nthString nTuple n = d : nthStringBase (n-o) (otherCiphersBound+1) (nTuple-1)
	where (d,o) = firstDigitAndOffset nTuple n

instances :: Int -> ([Int],Int) -> [[Int]]
instances r (offset, len) = take len $ ct2 offset

problemSize :: Int -> Int
problemSize nTuple = snd $ last $ firstDigitAndOffsets nTuple

tasks :: Int -> Int -> [([Int],Int)]
tasks n p = zip (map (nthString n) os) xs
	where
		total = problemSize n
		l = total `div` p
		o = total `mod` p
		xs = (take o $ repeat (l+1)) ++ (take (p-o) $ repeat l)
		os = scanl (+) 0 xs

{-
Følgende er ens: 
let (m,r)=(4,0) in length $ ct2 r $ take m $ repeat 0
let (m,r)=(4,0) in length $ concat $ map (instances r) $ tasks r m 10
let (m,r)=(4,0) in problemSize r m

Følgende er sand:
let (m,r)=(4,0) in (ct2 r $ take m $ repeat 0) == (map (nthString r m) [0..problemSize r m-1])

m er IKKE det samme m som i newSortingAlg etc.
-}