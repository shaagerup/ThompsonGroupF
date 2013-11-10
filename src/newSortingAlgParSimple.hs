import System.Environment
import System.IO

import BelkBrown
import qualified ThompsonParallelSimple

import Data.List.Split

{-
NB: For now, the parallel strategy is *not* faster than the serial! 
-}

b :: Int -> Generator
b = (!!) [X0, X1, X0Inv, X1Inv]

conv :: [Int] -> [Generator]
conv = map b


f' :: Int -> [Int]
f' x = [[0,1,3], [0,1,2], [1,2,3], [0,2,3]]!!x
f :: Int -> Int -> Int
f x y = (f' x)!!y

{- Konvertering fra simpel tuppel til den der faktisk skal bruges -}
convertTuple' :: Int -> [Int] -> [Int]
convertTuple' x (y:ys) = z : convertTuple' z ys where z = f x y
convertTuple' _ [] = []
convertTuple :: [Int] -> [Int]
convertTuple (x:xs) = x:convertTuple' x xs
---

tasks :: Int -> Int -> [([Int],Int)]
tasks m p = ThompsonParallelSimple.tasks m p

instances :: Int -> ([Int],Int) -> [[Generator]]
instances n task =  map (conv . convertTuple) $ ThompsonParallelSimple.instances 0 task


main :: IO ()
main = do
	(argStr:_) <- getArgs
	let 
		nStr:pStr:tStr:[] = splitOn "_" argStr
		m = (read nStr :: Int)
		p = (read pStr :: Int)
		t = (read tStr :: Int)
		ts = tasks m p
	mapM_ putStrLn $ map show $ map evaluate . instances m $ ts!!t