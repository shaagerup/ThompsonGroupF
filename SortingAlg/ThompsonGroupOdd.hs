{-
New algorithm related to the Thompson Group
-}
import Control.Parallel.Strategies
import System.Environment
import System.IO

b :: Int -> Int -> Int
b 0 n = [2*n, (n+3) `div`2, 2*n, n, 2*n, (n-3) `div` 2, 2*n, n]!!(n `mod` 8)
b 1 n = [n `div` 2, 2*n+1, n-1, 2*n+1]!!(n `mod` 4)
b 2 n = [n, 2*n+4, 2*n-2, (n-1) `div` 2]!!(n `mod` 4)
b 3 n = [n `div` 2, 2*n+3, 2*n-3, n]!!(n `mod` 4)
b 4 n = [2*n, n+1, 2*n, (n-1) `div` 2]!!(n `mod` 4)
b 5 n = [n, 2*n+1, (n+2) `div` 2, 2*n+1, n, 2*n+1,(n-4) `div` 2, 2*n+1]!!(n `mod` 8)

c :: Int -> Int -> Int
c 0 = b 5
c 1 = id
c 2 = b 0

f' :: Int -> [Int]
f' x = [[0,2,4,5],[0,1,3,5],[1,2,3,4],[1,2,3,4],[0,2,4,5],[0,1,3,5]]!!x
f :: Int -> Int -> Int
f x y = (f' x)!!y

g' :: Int -> [Int]
g' x = [[0,1,3,5],[1,2,3,4],[0,2,4,5]]!!x
g :: Int -> Int -> Int
g x y = (g' x)!!y


evalFunk :: (Int -> Int -> Int) -> Int -> [Int] -> Int
evalFunk a n [] 		= n
evalFunk a n (x:xs) 	= a x (evalFunk a n xs)

evalFunk' :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
evalFunk' a b n (x:xs)  = b x (evalFunk a n xs)

evaluate :: Int -> [Int] -> [Int]
evaluate n xs = map (\m -> evalFunk' b c m xs) [0..nUpperbound-1] where nUpperbound = 2^(n + 1)

{- tuples of length n in base b-}

tuplesByBase :: Int -> Int -> [[Int]]
tuplesByBase _ 0 = [[]]
tuplesByBase b n = concat [ map (i:) $ tuplesByBase b (n-1) | i <- [0..b-1]]

{- Første ciffer er fra 0..5, resten er fra 0..3 -}
specialTuples :: Int -> [[Int]]
specialTuples n = concat [ map (i:) $ tuplesByBase 4 n | i <- [0..2]]

{- Konvertering fra simpel tuppel til den der faktisk skal bruges -}
convertTuple' :: Int -> [Int] -> [Int]
convertTuple' x (y:ys) = z : convertTuple' z ys where z = f x y
convertTuple' _ [] = []
convertTuple :: [Int] -> [Int]
convertTuple (x:y:xs) = x:(g x y):convertTuple' (g x y) xs

main :: IO ()
main = do
	(nStr:_) <- getArgs
	let 
		n = (read nStr :: Int)
		res = specialTuples n
	mapM_ putStrLn $ map show $ map (evaluate n) $ map convertTuple $ res