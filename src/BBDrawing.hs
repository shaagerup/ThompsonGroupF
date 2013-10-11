module BBDrawing where

import BelkBrown
import Data.List

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
convertForest (Forest i ts) = VForest i (snd $ convertTreeList p p ts)
	where p = (0,0)

convertTreeList :: Point Int -> Point Int -> [Tree] -> (Point Int,[VTree (Point Int)])
convertTreeList nextLeaf p [] = (nextLeaf,[])
convertTreeList nextLeaf p (t:ts) = (nextLeaf'',t' : ts')
	where 
		(nextLeaf',t') = convertTree nextLeaf p t
		p' = (o1+1,o2+1)
		(nextLeaf'', ts') = convertTreeList nextLeaf' p' ts 
		(o1,o2) = getValue t'

convertTree :: Point Int ->  Point Int -> Tree -> (Point Int,VTree (Point Int))
convertTree nextLeaf p (N t1 t2) = (nextLeaf'',VN p' t1' t2')
	where 
		(nextLeaf', t1') = convertTree nextLeaf p t1
		(o11,o21) = getValue t1'
		(nextLeaf'', t2') = convertTree nextLeaf' (o11+1,o21+1) t2
		(o12,o22) = getValue t2'
		p' = (o12,o21)
convertTree nextLeaf@(x,y) p L = ((x+1,y+1),VL nextLeaf)

-- skal P altid være den sidst placerede Leaf ??

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
	where 
		--f (x,y) = (fromIntegral x, fromIntegral y)
		f = scalePoint (1 / sqrt 2) . rotatePoint (-pi/4)
	
getFDiagramDrawing :: FDiagram -> VFDiagram (Point Float)
getFDiagramDrawing (f1,f2) = (c1 f1, c2 f2)
	where
		c1 = transformForest (\(x,y) -> (x,0.2-y)) . getForestDrawing
		c2 = transformForest (\(x,y) -> (x,-(0.2-y))) . getForestDrawing
		
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

getBoundsJSON :: ((Float,Float),(Float,Float)) -> String
getBoundsJSON (lbound,ubound) = wrap $ concat $ intersperse "," $ map tupleJSON [lbound,ubound] where wrap x = "["++ x ++"]"
		
getDiagramBounds :: VFDiagram (Point Float) -> ((Float,Float),(Float,Float))
getDiagramBounds = getBounds . getValues

getJSON :: VFDiagram (Float,Float) -> String
getJSON (f1,f2) = "[" ++ (getForestJSON f1) ++ "," ++ (getForestJSON f2) ++ "]"

getForestJSON :: VForest (Float,Float) -> String
getForestJSON (VForest p ts) = "["++ (show p)++","++ (getTreeListJSON ts) ++ "]"

getTreeListJSON :: [VTree (Float,Float)] -> String
getTreeListJSON =  wrap . concat . (intersperse ",") . (map getTreeJSON)
	where wrap x = "["++x++"]"

getTreeJSON :: VTree (Float,Float) -> String
getTreeJSON (VL v) = tupleJSON v
getTreeJSON (VN v t1 t2) = "["++ (concat $ intersperse "," [tupleJSON v, getTreeJSON t1, getTreeJSON t2]) ++"]"

tupleJSON :: (Float,Float) -> String
tupleJSON (x,y) = "{\"x\":"++ (show x)++", \"y\":"++(show y)++"}"
