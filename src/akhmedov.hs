import BelkBrown
import System.Environment

inv' :: Generator -> Generator
inv' X0 = X0Inv
inv' X0Inv = X0
inv' X1 = X1Inv
inv' X1Inv = X1

inv :: [Generator] -> [Generator]
inv = reverse . (map inv')

pow :: Int -> Generator  -> [Generator]
pow n
 | n >= 0 = take n . repeat
 | otherwise = inv . pow (-n)
 

x :: Int -> [Generator]
x 0 = [X0]
x n = (pow (1-n) X0) ++ X1 : (pow (n-1) X0)

data Alphabet = A | B | C | D | E | F deriving (Eq,Show)
g' :: Alphabet -> [Generator]
g' A = x 3 ++ inv (x 1)
g' B = x 4 ++ inv (x 1)
g' C = x 1 ++ inv (x 3)
g' D = x 1 ++ inv (x 4)
g' E = x 4 ++ inv (x 2)
g' F = x 2 ++ inv (x 4)

g :: [Alphabet] -> [Generator]
g = concat . map g'

cond :: [Alphabet] -> Bool
cond xs = (and [ not $ p `elem` ps | p <- forbiddenPairs]) && ((not $ E `elem` xs) || (not $ F `elem` xs))
	where 
		ps = zip xs (tail xs)
		forbiddenPairs = [(A,C), (C,A), (B,D), (D,B), (E,F), (F,E), (F,C), (C,B), (D,A)]

ws 0 = [[]]
ws n = [ x:xs | x <- [A,B,C,D,E,F], xs <- ws (n-1)]

us = filter cond . ws