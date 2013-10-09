module BBDrawing where

import BelkBrown

data VForest a = VForest Int [VTree a] deriving (Eq,Show)
data VTree a = VN a (VTree a) (VTree a) | VL a deriving (Eq,Show)
type Point a = (a,a)
type VFDiagram a = (VForest a,VForest a)

getValue :: VTree a -> a
getValue (VN v _ _) = v
getValue (VL v) = v

--convertTree :: Tree -> VTree (Point Int)
--convertTree t = convertTree' (Point 0 l) t
--	where l = leafCount t

convertForest :: Forest -> VForest (Point Int)
convertForest (Forest i ts) = VForest i (convertTreeList p ts)
	where p = (0,0)

convertTreeList :: Point Int -> [Tree] -> [VTree (Point Int)]
convertTreeList p [] = []
convertTreeList p (t:ts) = t' : convertTreeList p' ts 
	where 
		t' = convertTree p t
		(o1,o2) = getValue t'
		p' = (o1+1,o2+1)

convertTree :: Point Int -> Tree -> VTree (Point Int)
convertTree p (N t1 t2) = VN p' t1' t2' 
	where 
		t1' = convertTree p t1
		(o11,o21) = getValue t1'
		t2' = convertTree (o11+1,o21+1) t2
		(o12,o22) = getValue t2'
		p' = (o12,o21)
convertTree p L = VL p
{-
algoritme: 
i hver Node: 
	descend til venstre
	descend til højre
	gem i node
	returnér
-}

rotatePoint :: Float -> (Int,Int) -> (Float,Float)
rotatePoint theta (x,y) = (x',y')
	where 
		x' = (fromIntegral x) * (cos theta) - (fromIntegral y) * (sin theta)
		y' = (fromIntegral x) * (sin theta) + (fromIntegral y) * (cos theta)
		
scalePoint :: Float -> (Float,Float) -> (Float,Float)
scalePoint s (x,y) = (s*x, s*y)

transformForest :: (a -> b) -> VForest a -> VForest b
transformForest f (VForest i ts) = VForest i (transformTreeList f ts)

transformTreeList :: (a -> b) -> [VTree a] -> [VTree b]
transformTreeList f [] = []
transformTreeList f (t:ts) = transformTree f t : transformTreeList f ts

transformTree :: (a -> b) -> VTree a -> VTree b
transformTree f (VL p) = VL (f p)
transformTree f (VN p t1 t2) = VN (f p) (transformTree f t1) (transformTree f t2)

getForestDrawing :: Forest -> VForest (Point Float)
getForestDrawing = transformForest f . convertForest
	where f = scalePoint (1 / sqrt 2) . rotatePoint (-pi/4)
	
getFDiagramDrawing :: FDiagram -> VFDiagram (Point Float)
getFDiagramDrawing (f1,f2) = (c1 f1, c2 f2)
	where
		c1 = transformForest (\(x,y) -> (x,1+y)) . getForestDrawing
		c2 = transformForest (\(x,y) -> (x,-(1+y))) . getForestDrawing
		
getValues :: VFDiagram a -> [a]
getValues (f1,f2) = getValuesForest f1 ++ getValuesForest f2

getValuesForest :: VForest a -> [a]
getValuesForest (VForest _ ts) = getValuesTreeList ts

getValuesTreeList :: [VTree a] -> [a]
getValuesTreeList [] = []
getValuesTreeList (t:ts) = getValuesTree t ++ getValuesTreeList ts

getValuesTree :: VTree a -> [a]
getValuesTree (VL v) = [v]
getValuesTree (VN v t1 t2) = getValuesTree t1 ++ v : getValuesTree t2

getBounds :: [(Float,Float)] -> ((Float,Float),(Float,Float))
getBounds ts = (lbound, ubound)
	where
		xs = map fst ts
		ys = map snd ts
		lbound = (minimum xs, minimum ys)
		ubound = (maximum xs, maximum ys)
		
getDiagramBounds :: VFDiagram (Point Float) -> ((Float,Float),(Float,Float))
getDiagramBounds = getBounds . getValues