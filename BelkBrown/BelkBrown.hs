{-
This program implements the representation of the Thompson Group F given in 
Section 3.2 (Forest Diagrams for Elements of PL2(R))
of http://arxiv.org/pdf/math/0305412v1.pdf
-}
import Data.List
import Data.Tuple

data Generator = X0 | X1 | X0Inv | X1Inv deriving (Eq,Show)
data Tree = Node Tree Tree | Leaf deriving (Eq,Show)
data Forest = Forest Int [Tree] deriving (Eq,Show)
type FDiagram = (Forest,Forest) 
-- first is domain, second is range. I.e. first is the bottom forest, second is the top forest! 
-- When working with FDiagram, we assume that both forests have equally many leafs.




leafCount :: Tree -> Int
leafCount Leaf = 1
leafCount (Node t1 t2) = leafCount t1 + leafCount t2

fLeafCount :: [Tree] -> Int
fLeafCount ts = sum $ map leafCount $ ts

op :: Generator -> FDiagram -> FDiagram
op X0 (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2' xs2')
	where 
		p2' = p2+1
		(xs1',xs2')
		 | p2' >= (length xs2) = (xs1 ++ [Leaf], xs2 ++ [Leaf])
		 | otherwise = (xs1,xs2)

op X1 (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2 xs2')
	where 
		(ys1,ys2) = splitAt p2 xs2
		(t1:t2:ys3) = if length ys2 == 1 then ys2++[Leaf] else ys2
		xs2' = ys1 ++ [Node t1 t2] ++ ys3
		xs1'
		 | fLeafCount xs2' > fLeafCount xs1 = xs1++[Leaf]
		 | otherwise = xs1

op X0Inv (Forest p1 xs1, Forest p2 xs2) = (Forest p1' xs1', Forest p2' xs2')
	where
		p1' = if p2>0 then p1 else p1+1
		xs1' = if p2>0 then xs1 else [Leaf]++xs1
		p2' = if p2>0 then p2-1 else p2
		xs2' = if p2>0 then xs2 else [Leaf]++xs2

op X1Inv (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2 xs2')
	where 
		e = xs2!!p2
		xs2e = case e of 
			Leaf -> [Leaf, Leaf]
			Node t1 t2 -> [t1,t2]
		xs2' = take p2 xs2 ++ xs2e ++ drop (p2+1) xs2
		xs1' = case e of 
			Leaf -> snd $ modifyNthLeafPlural p1 xs1 (Node Leaf Leaf)
			_ -> xs1
		--xs1''
		-- | fLeafCount xs2' > fLeafCount xs1' = xs1' ++ [Leaf]
		-- | otherwise = xs1'
		-- find det p1'e blad og extend dette! 
		
-- replaces nth leaf w. l'
modifyNthLeaf :: Int -> Tree -> Tree -> (Int,Tree)
modifyNthLeaf n Leaf l'
 | n == 0 = (n-1, l')
 | n /= 0 = (n-1, Leaf)
modifyNthLeaf n (Node t1 t2) l' = (n'', Node t1' t2')
	where 
		(n',t1') = modifyNthLeaf n t1 l'
		(n'', t2') = modifyNthLeaf n' t2 l'

modifyNthLeafPlural :: Int -> [Tree] -> Tree -> (Int, [Tree])
modifyNthLeafPlural n [] l' = (n,[])
modifyNthLeafPlural n (t:ts) l' = (n'', t':ts')
	where 
		(n',t') = modifyNthLeaf n t l'
		(n'',ts') = modifyNthLeafPlural n' ts l'

carets :: Int -> Tree -> [Int] -> (Int,[Int])
carets n (Leaf) cs = (n+1,cs)
carets n (Node Leaf Leaf) cs = (n+2, n:cs)
carets n (Node t1 t2) cs = (n'',cs'')
	where 
		(n',cs') = carets n t1 cs
		(n'',cs'') = carets n' t2 cs'

caretsPlural :: Int -> [Tree] -> (Int,[Int])
caretsPlural n [] = (n,[])
caretsPlural n (t:ts) = (n'',cs'' ++ cs')
	where
		(n',cs') = carets n t []
		(n'', cs'') = caretsPlural n' ts

treelistCarets :: [Tree] -> [Int]
treelistCarets ts = reverse $ snd $ caretsPlural 0 ts

opposingCarets :: FDiagram -> [Int]
opposingCarets (Forest _ ts1, Forest _ ts2) = intersect (treelistCarets ts1) (treelistCarets ts2) 

removeNthCaret :: Int -> Tree -> (Int,Tree)
removeNthCaret n (Leaf) = (n-1,Leaf)
removeNthCaret n t@(Node Leaf Leaf) 
 | n==0 = (n-2, Leaf)
 | otherwise = (n-2, t)
removeNthCaret n (Node t1 t2) = (n'',Node t1' t2')
	where
		(n',t1') = removeNthCaret n t1
		(n'',t2') = removeNthCaret n' t2 

removeNthCaretPlural :: Int -> [Tree] -> (Int,[Tree])
removeNthCaretPlural n [] = (n,[])
removeNthCaretPlural n (t:ts) = (n'',t':ts')
	where
		(n',t') = removeNthCaret n t
		(n'',ts') = removeNthCaretPlural n' ts	

removeCaret :: Int -> Forest -> Forest
removeCaret n (Forest p ts) = Forest p ts'
	where
		ts' = snd $ removeNthCaretPlural n ts

removeCarets :: [Int] -> Forest -> Forest
removeCarets [] = id
removeCarets (c:cs) = (removeCarets cs) . (removeCaret c)

-- reduce deletes opposing pairs of carets, see BelkBrown page 10.
reduce :: FDiagram -> FDiagram
reduce d@(f1,f2) = (removeCarets cs f1, removeCarets cs f2)
	where cs = opposingCarets d


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

testAll :: Bool
testAll = and $ map test [1..8]

{-
TODO:
Apply reduce as part of operations + Trim excess leafs in the head and tail of the treelists.
-}