module BelkBrown where

{-
This program implements the representation of the Thompson Group F given in 
Section 3.2 (Forest Diagrams for Elements of PL2(R))
of http://arxiv.org/pdf/math/0305412v1.pdf
-}
import Data.List
import Data.Tuple
import Data.Serialize
import Data.Serialize.Get
import Control.Monad
import qualified Data.ByteString 

data Generator = X0 | X1 | X0Inv | X1Inv deriving (Eq,Show)
data Tree = N Tree Tree | L deriving (Eq,Show)
data Forest = Forest Int [Tree] deriving (Eq,Show)
type FDiagram = (Forest,Forest) 
-- first is domain, second is range. I.e. first is the bottom forest, second is the top forest! 
-- When working with FDiagram, we assume that both forests have equally many leafs.

instance Enum Generator where
    fromEnum            = \x -> case x of
							X0 -> 0
							X0Inv -> 1
							X1 -> 2
							X1Inv -> 3
    toEnum          = (!!) [X0,X0Inv,X1,X1Inv]
    enumFrom c        = map toEnum [fromEnum c .. fromEnum X1Inv]
    enumFromThen c c' = map toEnum [fromEnum c, fromEnum c' .. fromEnum lastChar]
                      where lastChar | fromEnum c' < fromEnum c    = X0
                                     | otherwise = X1Inv

instance Serialize Generator where
    put     = putWord8 . fromIntegral . fromEnum
    get     = liftM (toEnum . fromIntegral) getWord8

putTree :: Putter Tree
putTree (N t1 t2) = putListOf putTree [t1,t2]
putTree L = putListOf putTree []

getTree :: Get Tree
getTree = do
	l <- getListOf getTree
	return $ case l of
		[] -> L
		[t1,t2] -> N t1 t2

instance Serialize Tree where
    put = putTree
    get = getTree

instance Serialize Forest where
	put (Forest p ts) = put p >> putListOf put ts
	get = do
		p <- get
		ts <- getListOf get
		return $ Forest p ts


encodeDiagram :: FDiagram -> Data.ByteString.ByteString
encodeDiagram d = Data.ByteString.concat [d',l] 
	where
		d' = encode d
		l = encode $ (Data.ByteString.length d') `div` 8
		

leafCount :: Tree -> Int
leafCount L = 1
leafCount (N t1 t2) = leafCount t1 + leafCount t2

fLeafCount :: [Tree] -> Int
fLeafCount ts = sum $ map leafCount $ ts

op :: Generator -> FDiagram -> FDiagram
op X0 (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2' xs2')
	where 
		p2' = p2+1
		(xs1',xs2')
		 | p2' >= (length xs2) = (xs1 ++ [L], xs2 ++ [L])
		 | otherwise = (xs1,xs2)

op X1 (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2 xs2')
	where 
		(ys1,ys2) = splitAt p2 xs2
		(t1:t2:ys3) = case (length ys2) of
			1 -> ys2++[L]
			_ -> ys2
		xs2' = ys1 ++ [N t1 t2] ++ ys3
		xs1'
		 | fLeafCount xs2' > fLeafCount xs1 = xs1++[L]
		 | otherwise = xs1

op X0Inv (Forest p1 xs1, Forest p2 xs2) = (Forest p1' xs1', Forest p2' xs2')
	where
		p1' = if p2>0 then p1 else p1+1
		xs1' = if p2>0 then xs1 else [L]++xs1
		p2' = if p2>0 then p2-1 else p2
		xs2' = if p2>0 then xs2 else [L]++xs2

op X1Inv (Forest p1 xs1, Forest p2 xs2) = (Forest p1 xs1', Forest p2 xs2')
	where 
		e = xs2!!p2
		xs2e = case e of 
			L -> [L, L]
			N t1 t2 -> [t1,t2]
		xs2' = take p2 xs2 ++ xs2e ++ drop (p2+1) xs2
		xs1' = case e of 
			L -> snd $ modifyNthLeafPlural (fLeafCount $ take p2 xs2) xs1 (N L L)
			_ -> xs1


op' :: Generator -> FDiagram -> FDiagram
op' X0 = trimHead . (op X0)
op' X1 = reduce . (op X1)
op' X0Inv = trimTail . (op X0Inv)
op' X1Inv = trimTail . reduce . (op X1Inv)



-- replaces nth leaf with l'
modifyNthLeaf :: Int -> Tree -> Tree -> (Int,Tree)
modifyNthLeaf n L l'
 | n == 0 = (n-1, l')
 | n /= 0 = (n-1, L)
modifyNthLeaf n (N t1 t2) l' = (n'', N t1' t2')
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
carets n (L) cs = (n+1,cs)
carets n (N L L) cs = (n+2, n:cs)
carets n (N t1 t2) cs = (n'',cs'')
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
removeNthCaret n (L) = (n-1,L)
removeNthCaret n t@(N L L) 
 | n==0 = (n-2, L)
 | otherwise = (n-2, t)
removeNthCaret n (N t1 t2) = (n'',N t1' t2')
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

-- reduce deletes opposing pairs of carets, see BelkBrown page 10.
reduce :: FDiagram -> FDiagram
reduce d@(f1,f2) = case cs of 
		[] -> d
		c:_ -> reduce (removeCaret c f1, removeCaret c f2)
	where cs = opposingCarets d

{- Trim functions for removal of excess leafs in the head and tail of the treelists. -}
mirror :: FDiagram -> FDiagram
mirror (Forest p1 ts1, Forest p2 ts2) = (Forest (length ts1 - p1 - 1) (reverse ts1),Forest (length ts2 - p2 - 1) (reverse ts2))

trimHead :: FDiagram -> FDiagram
trimHead d@(Forest p1 (L:ts1), Forest p2 (L:ts2))
 | p1 > 0 && p2 > 0 = (Forest (p1-1) ts1, Forest (p2-1) ts2)
 | otherwise = d
trimHead d = d

trimTail :: FDiagram -> FDiagram
trimTail = mirror . trimHead . mirror


--main = putStrLn $ show $ length $ trivialReps 14
--main = mapM_ putStrLn $ map (show . length . trivialReps) [1..]

trivialDiagram :: FDiagram
trivialDiagram = (Forest 0 [L], Forest 0 [L]) 

evaluate :: [Generator] -> FDiagram
evaluate = foldr op' trivialDiagram

isTrivial :: FDiagram -> Bool
isTrivial = (==) trivialDiagram

{-
elems' :: Generator -> Int -> [[Generator]]
elems' _ 0 = [[]]
elems' X0 n = concat [map (e:) $elems' e (n-1) | e <- [X0,X1,X1Inv]]
elems' X1 n = concat [map (e:) $elems' e (n-1) | e <- [X0,X1,X0Inv]]
elems' X0Inv n = concat [map (e:) $elems' e (n-1) | e <- [X1,X0Inv,X1Inv]]
elems' X1Inv n = concat [map (e:) $elems' e (n-1) | e <- [X0,X0Inv,X1Inv]]

elems :: Int -> [[Generator]]
elems n = concat [map (e:) $elems' e (n-1) | e <- [X0,X1,X0Inv,X1Inv]]

trivialReps :: Int -> [[Generator]]
trivialReps = filter (isTrivial . evaluate) . elems
-}