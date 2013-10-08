import System.Environment
import System.IO

import qualified ThompsonGroup
import qualified ThompsonGroupOdd

evaluate :: Int -> [Int] -> [Int]
evaluate n = case r of
	0 -> ThompsonGroup.evaluate m
	1 -> ThompsonGroupOdd.evaluate m
	where
		m = n `div` 2
		r = n `mod` 2

elems :: Int -> [[Int]]
elems n = case r of
	0 -> ThompsonGroup.elems m
	1 -> ThompsonGroupOdd.elems m
	where
		m = n `div` 2
		r = n `mod` 2

main :: IO ()
main = do
	(nStr:_) <- getArgs
	let 
		n = (read nStr :: Int)
	mapM_ putStrLn $ map show $ map (evaluate n) $ elems n