import System.IO
import System.Environment
getRunLengths [] = []
getRunLengths (x:xs) = f 1 x xs
	where
		f c p [] = [c]
		f c p (x:xs)
		 | p==x = f (c+1) x xs
		 | otherwise = c : f 1 x xs

func content = (show $ sum $ map (\x -> x*x) $ getRunLengths $ lines content) ++ "\n"

main = do
	interact func