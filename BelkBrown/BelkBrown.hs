data Generator = X0 | X1 | X0Inv | X1Inv deriving Show

data Tree = Node Tree Tree | Leaf deriving Show
data Forest = Forest Int [Tree] deriving Show

type FDiagram = (Forest,Forest) -- first is domain, second is range. I.e. first is the bottom forest, second is the top forest! 
-- When working with FDiagram, we assume that both forests have equally many leafs.

-- reduce deletes opposing pairs of carets, see BelkBrown page 10.
reduce :: FDiagram -> FDiagram
reduce = undefined


-- example forest diagram from page 10:
exDom = Forest 1 [Node (Node Leaf Leaf) (Node Leaf Leaf), Node Leaf Leaf]
exRan = Forest 1 [Leaf, Leaf, Node (Node Leaf Leaf) Leaf]
exDiag = (exDom,exRan)

leafCount :: Tree -> Int
leafCount Leaf = 1
leafCount (Node t1 t2) = leafCount t1 + leafCount t2

fLeafCount :: [Tree] -> Int
fLeafCount ts = sum $ map leafCount $ ts

op :: Generator -> FDiagram -> FDiagram
op X0 (Forest p1 xs1, Forest p2 xs2) = (Forest p1' xs1', Forest p2 xs2')
	where 
		p1' = p1+1
		(xs1',xs2')
		 | p1' > (length xs1) = (xs1 ++ [Leaf], xs2 ++ [Leaf])
		 | otherwise = (xs1,xs2)

op X1 (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2 xs2')
	where 
		(ys1,ys2) = splitAt p1 xs1
		(t1:t2:ys3) = if length ys2 == 1 then ys2++[Leaf] else ys2
		xs1' = ys1 ++ [Node t1 t2] ++ ys3
		xs2'
		 | fLeafCount xs1' > fLeafCount xs2 = xs2++[Leaf]
		 | otherwise = xs2

op X0Inv (Forest p1 xs1, Forest p2 xs2) = (Forest p1' xs1', Forest p2' xs2')
	where
		p1' = if p1>0 then p1-1 else p1
		xs1' = if p1>0 then xs1 else [Leaf]++xs1
		p2' = if p1>0 then p2 else p2+1
		xs2' = if p1>0 then xs2 else [Leaf]++xs2

op X1Inv (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2 xs2'')
	where 
		e = xs1!!p1
		xs1e = case e of 
			Leaf -> [Leaf, Leaf]
			Node t1 t2 -> [t1,t2]
		xs1' = take (p1-1) xs1 ++ xs1e ++ drop p1 xs1
		xs2' = case e of 
			Leaf -> snd $ modifyNthLeafPlural p1 xs2 (Node Leaf Leaf)
			_ -> xs2
		xs2''
		 | fLeafCount xs1' > fLeafCount xs2' = xs2' ++ [Leaf]
		 | otherwise = xs2'
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