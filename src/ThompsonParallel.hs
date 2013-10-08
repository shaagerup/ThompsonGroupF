module ThompsonParallel where

firstCipherBound :: Int -> Int
firstCipherBound 0 = 5
firstCipherBound 1 = 2

ct2'' :: Bool -> [Int] -> [[Int]]
ct2'' useOffset [] = [[]]
ct2'' useOffset (o:os) = [i : xs | i <- [(if useOffset then o else 0)..3], xs <- (ct2'' (useOffset && i==o) os)]
ct2' r useOffset (o:os) = [i : xs | i <- [(if useOffset then o else 0)..firstCipherBound r], xs <- (ct2'' (useOffset && i==o) os)] -- sæt første element som offset ved de senere!
ct2 r (o:os) = ct2' r True (o:os)

nthStringBase' :: Int -> Int -> [Int]
nthStringBase' 0 b = []
nthStringBase' n b = n `mod` b : nthStringBase' (n `div` b) b

nthStringBase :: Int -> Int -> Int -> [Int]
nthStringBase n b l = (take (l - (length xs)) (repeat 0)) ++ reverse xs
	where xs = nthStringBase' n b

firstDigitAndOffsets :: Int -> Int -> [(Int,Int)]
firstDigitAndOffsets r nTuple = scanl (\(j1,v1) (j2,v2) -> (j2, v1+v2)) (0,0) [(i ,4^(nTuple - 1)) | i<- [1..firstCipherBound r + 1] ]

firstDigitAndOffset :: Int -> Int -> Int -> (Int,Int)
firstDigitAndOffset r nTuple n = last $ takeWhile (\(_,v) -> v <= n) $ firstDigitAndOffsets r nTuple

nthString :: Int -> Int -> Int -> [Int]
nthString r nTuple n = d : nthStringBase (n-o) 4 (nTuple-1)
	where (d,o) = firstDigitAndOffset r nTuple n

instances :: Int -> ([Int],Int) -> [[Int]]
instances r (offset, len) = take len $ ct2 r offset

problemSize :: Int -> Int -> Int
problemSize r nTuple = snd $ last $ firstDigitAndOffsets r nTuple

tasks :: Int -> Int -> Int -> [([Int],Int)]
tasks r n p = zip (map (nthString r n) os) xs
	where
		total = problemSize r n
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