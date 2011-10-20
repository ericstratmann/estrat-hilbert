import HilbertTree
import System.Environment

main :: IO ()
main = do
    fileRectangles <- readRectanglesFile
    inputRectangles <- readRectanglesStdin
    let tree = buildTree fileRectangles
    let _n = fmap (searchTree tree) inputRectangles
    return ()

readRectanglesStdin :: IO [Rectangle]
readRectanglesStdin = do
    input <- getContents
    let rectangles = parseRectangles input
    return rectangles

readRectanglesFile :: IO [Rectangle]
readRectanglesFile = do 
    args <- getArgs
    if null args
        then
            return []
        else do
            input <- readFile $ head args
            let rectangles = parseRectangles input
            return rectangles

parseRectangles :: String -> [Rectangle]
parseRectangles rects = buildRectangles $ fmap read $ split ',' rects

buildRectangles :: [Integer] -> [Rectangle]
buildRectangles (x1:y1:x2:y2:x3:y3:x4:y4:_:rest) = Rectangle xMin xMax yMin yMax : buildRectangles rest
    where
    xMin = foldl1 min xs
    xMax = foldl1 max xs
    yMin = foldl1 min ys
    yMax = foldl1 max ys
    xs = [x1, x2, x3, x4]
    ys = [y1, y2, y3, y4]
buildRectangles _ = []

split :: Char -> String -> [String]
split c s
  | findDelim == [] = []
  | otherwise = w : split c s'' where
      (w, s'') = break (c==) findDelim
      findDelim = dropWhile (c==) s
