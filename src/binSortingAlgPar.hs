import System.Environment
import System.IO

import BelkBrown
import qualified ThompsonGroup
import qualified ThompsonGroupOdd
import qualified ThompsonParallel

import Data.List.Split
import qualified Data.ByteString 

{-
NB: For now, the parallel strategy is *not* faster than the serial! 
-}

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


---

tasks :: Int -> Int -> [([Int],Int)]
tasks n p = case r of
	0 -> ThompsonParallel.tasks 0 m p
	1 -> ThompsonParallel.tasks 1 (m+1) p
	where
		m = n `div` 2
		r = n `mod` 2

instances :: Int -> ([Int],Int) -> [[Generator]]
instances n task = case r of
	0 -> map (conv . ThompsonGroup.convertTuple) $ ThompsonParallel.instances 0 task
	1 -> map (convOdd . ThompsonGroupOdd.convertTuple) $ ThompsonParallel.instances 1 task
	where
		m = n `div` 2
		r = n `mod` 2

{-
Note that the following holds for all n,p
let (n,p) = (4,1) in (concat $ map (instances n) $ tasks n p) == elems n
-}

elems :: Int -> [[Generator]]
elems n = case r of
	0 -> map conv $ ThompsonGroup.elems m
	1 -> map convOdd $ ThompsonGroupOdd.elems m
	where
		m = n `div` 2
		r = n `mod` 2

main :: IO ()
main = do
	(argStr:_) <- getArgs
	let 
		nStr:pStr:tStr:[] = splitOn "_" argStr
		n = (read nStr :: Int)
		p = (read pStr :: Int)
		t = (read tStr :: Int)
		ts = tasks n p
	mapM_ Data.ByteString.putStr $ map encodeDiagram $ map evaluate . instances n $ ts!!t