import System.Environment
import System.IO

import BelkBrown
import qualified ThompsonGroup
import qualified ThompsonGroupOdd

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

elems :: Int -> [[Generator]]
elems n = case r of
	0 -> map conv $ ThompsonGroup.elems m
	1 -> map convOdd $ ThompsonGroupOdd.elems m
	where
		m = n `div` 2
		r = n `mod` 2


main :: IO ()
main = do
	(nStr:_) <- getArgs
	let 
		n = (read nStr :: Int)
	mapM_ putStrLn $ map show $ map evaluate $ elems n