import BelkBrown
import BBDrawing
import Data.List
{-
helping functions
-}
--rotations xs = take (length xs) $ zipWith (++) (tails xs) (inits xs) 


-- example forest diagrams
ex332diag = (exDom, exRan)
	where
		exDom = Forest 1 [N (N L L) (N L L), L, L]
		exRan = Forest 1 [L, N L L, N (N L L) L]

ex333diag = (exDom, exRan)
	where
		exDom = Forest 0 [N (N L L) (N L L), L]
		exRan = Forest 1 [N (N L L) L, N L L]

ex334diag = (exDom, exRan)
	where
		exDom = Forest 1 [L,L,L,N L (N L L), L, L]
		exRan = Forest 1 [N (N L L) (N L L), L, L, N L L]

ex336diagf = (exDom, exRan)
	where
		exDom = Forest 0 [L, N (N L L) L]
		exRan = Forest 0 [N (N L L) (N L L)]

ex336diagg = (exDom, exRan)
	where
		exDom = Forest 1 [N (N L L) L, L]
		exRan = Forest 1 [L, L, N L L]


b :: Int -> [Generator]
b 0 = [X1Inv, X0]
b 1 = [X0Inv]
b 2 = [X1]
b 3 = [X0Inv, X1]
b 4 = [X0]
b 5 = [X1Inv]

c :: Int -> [Generator]
c 0 = b 5
c 1 = []
c 2 = b 0

conv :: [Int] -> [Generator]
conv = concat . map b

convOdd :: [Int] -> [Generator]
convOdd (x:xs) = c x ++ conv xs


test :: Int -> Bool
test 1 = reduce exDiag == exDiag'
	where
		exDom = Forest 1 [N (N L L) (N L L), N L L]
		exRan = Forest 1 [L, L, N (N L L) L, L]
		exDiag = (exDom,exRan)
		exDom' = Forest 1 [N (N L L) L,N L L]
		exRan' = Forest 1 [L,L,N L L,L]
		exDiag' = (exDom',exRan')

test 2 = op X0 ex332diag == (exDom, exRan)
	where
		(exDom,_) = ex332diag
		exRan = Forest 2 [L, N L L, N (N L L) L]

test 3 = op X1 ex332diag == (exDom, exRan)
	where
		(exDom,_) = ex332diag
		exRan = Forest 1 [L, N (N L L) (N (N L L) L)]

test 4 = op X0 ex333diag == (exDom, exRan)
	where
		exDom = Forest 0 [N (N L L) (N L L), L, L]
		exRan = Forest 2 [N (N L L) L, N L L, L]

test 5 = op X1 ex333diag == (exDom, exRan)
	where
		exDom = Forest 0 [N (N L L) (N L L), L, L]
		exRan = Forest 1 [N (N L L) L, N (N L L) L]

test 6 = reduce (op X1 ex334diag) == (exDom, exRan)
	where
		exDom = Forest 1 [L,L,L,N L L, L, L]
		exRan = Forest 1 [N (N L L) (N L L), L, N L L]

test 7 = (op X1Inv ex336diagf) == (exDom, exRan)
	where
		(exDom,_) = ex336diagf
		exRan = Forest 0 [N L L, N L L]

test 8 = op X1Inv ex336diagg == (exDom, exRan)
	where
		exDom = Forest 1 [N (N L (N L L)) L, L]
		exRan = Forest 1 [L, L, L, N L L]

{-
B0B1B2 = I
B5B4B3 = I
B0B3 = B1B4 = B2B5 = I
-}

test 9 = trivialDiagram == (evaluate $ conv $ [0,1,2])
test 10 = trivialDiagram == (evaluate $ conv $ [5,4,3])
test 11 = trivialDiagram == (evaluate $ conv $ [0,3])
test 12 = trivialDiagram == (evaluate $ conv $ [1,4])
test 13 = trivialDiagram == (evaluate $ conv $ [2,5])

testAll :: Bool
testAll = and $ map test [1..13]
