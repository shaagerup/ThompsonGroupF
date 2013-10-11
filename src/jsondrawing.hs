import BelkBrown
import BBDrawing
import System.Environment
main = do
	(strxs:_) <- getArgs
	let
		xs = (read strxs :: [Int])
		drawing = getFDiagramDrawing $ evaluate $ map ([X0,X0Inv,X1,X1Inv]!!) xs
		js1 = getBoundsJSON $ getBounds $ getValues drawing
		js2 = getJSON drawing
	putStrLn $ "[" ++ js1 ++ "," ++ js2 ++ "]"