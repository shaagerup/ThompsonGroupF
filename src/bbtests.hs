import BelkBrown
import Data.List
{-
helping functions
-}
--rotations xs = take (length xs) $ zipWith (++) (tails xs) (inits xs) 


-- example forest diagrams
ex332diag = (exDom, exRan)
	where
		exDom = Forest 1 [Node (Node Leaf Leaf) (Node Leaf Leaf), Leaf, Leaf]
		exRan = Forest 1 [Leaf, Node Leaf Leaf, Node (Node Leaf Leaf) Leaf]

ex333diag = (exDom, exRan)
	where
		exDom = Forest 0 [Node (Node Leaf Leaf) (Node Leaf Leaf), Leaf]
		exRan = Forest 1 [Node (Node Leaf Leaf) Leaf, Node Leaf Leaf]

ex334diag = (exDom, exRan)
	where
		exDom = Forest 1 [Leaf,Leaf,Leaf,Node Leaf (Node Leaf Leaf), Leaf, Leaf]
		exRan = Forest 1 [Node (Node Leaf Leaf) (Node Leaf Leaf), Leaf, Leaf, Node Leaf Leaf]

ex336diagf = (exDom, exRan)
	where
		exDom = Forest 0 [Leaf, Node (Node Leaf Leaf) Leaf]
		exRan = Forest 0 [Node (Node Leaf Leaf) (Node Leaf Leaf)]

ex336diagg = (exDom, exRan)
	where
		exDom = Forest 1 [Node (Node Leaf Leaf) Leaf, Leaf]
		exRan = Forest 1 [Leaf, Leaf, Node Leaf Leaf]


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
		exDom = Forest 1 [Node (Node Leaf Leaf) (Node Leaf Leaf), Node Leaf Leaf]
		exRan = Forest 1 [Leaf, Leaf, Node (Node Leaf Leaf) Leaf, Leaf]
		exDiag = (exDom,exRan)
		exDom' = Forest 1 [Node (Node Leaf Leaf) Leaf,Node Leaf Leaf]
		exRan' = Forest 1 [Leaf,Leaf,Node Leaf Leaf,Leaf]
		exDiag' = (exDom',exRan')

test 2 = op X0 ex332diag == (exDom, exRan)
	where
		(exDom,_) = ex332diag
		exRan = Forest 2 [Leaf, Node Leaf Leaf, Node (Node Leaf Leaf) Leaf]

test 3 = op X1 ex332diag == (exDom, exRan)
	where
		(exDom,_) = ex332diag
		exRan = Forest 1 [Leaf, Node (Node Leaf Leaf) (Node (Node Leaf Leaf) Leaf)]

test 4 = op X0 ex333diag == (exDom, exRan)
	where
		exDom = Forest 0 [Node (Node Leaf Leaf) (Node Leaf Leaf), Leaf, Leaf]
		exRan = Forest 2 [Node (Node Leaf Leaf) Leaf, Node Leaf Leaf, Leaf]

test 5 = op X1 ex333diag == (exDom, exRan)
	where
		exDom = Forest 0 [Node (Node Leaf Leaf) (Node Leaf Leaf), Leaf, Leaf]
		exRan = Forest 1 [Node (Node Leaf Leaf) Leaf, Node (Node Leaf Leaf) Leaf]

test 6 = reduce (op X1 ex334diag) == (exDom, exRan)
	where
		exDom = Forest 1 [Leaf,Leaf,Leaf,Node Leaf Leaf, Leaf, Leaf]
		exRan = Forest 1 [Node (Node Leaf Leaf) (Node Leaf Leaf), Leaf, Node Leaf Leaf]

test 7 = (op X1Inv ex336diagf) == (exDom, exRan)
	where
		(exDom,_) = ex336diagf
		exRan = Forest 0 [Node Leaf Leaf, Node Leaf Leaf]

test 8 = op X1Inv ex336diagg == (exDom, exRan)
	where
		exDom = Forest 1 [Node (Node Leaf (Node Leaf Leaf)) Leaf, Leaf]
		exRan = Forest 1 [Leaf, Leaf, Leaf, Node Leaf Leaf]

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
