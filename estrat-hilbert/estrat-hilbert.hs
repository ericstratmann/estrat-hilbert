import HilbertTree
import System.Environment
import System.CPUTime

main :: IO ()
main = do
    start <- getCPUTime
    fileRectangles <- readRectanglesFile
    let tree = buildTree fileRectangles
    seq tree $ return ()
    end <- getCPUTime
    putStrLn $ show (length fileRectangles) ++ " rectangles read in " ++ getTimeDiff start end
    inputRectangles <- readRectanglesStdin
    mapM_ (findIntersections tree) inputRectangles
    return ()

findIntersections :: HilbertTree -> Rectangle -> IO ()
findIntersections tree rect = do
    start <- getCPUTime
    let rects = searchTree tree rect
    seq rects $ return ()
    end <- getCPUTime
    putStrLn $ "found " ++ show (length rects) ++ " matches in " ++ getTimeDiff start end
    mapM_ (\r -> putStrLn $ show r ++ "\n") (take 4 rects)

getTimeDiff :: Integer -> Integer -> String
getTimeDiff start end = show(div (end-start) 1000000000)  ++ " microseconds"

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
parseRectangles rects = buildRectangles $ fmap read $ split ',' $ map (\c -> if c == '\n' then ',' else c) rects

buildRectangles :: [Integer] -> [Rectangle]
buildRectangles (x1:y1:x2:y2:x3:y3:x4:y4:rest) = Rectangle xMin xMax yMin yMax : buildRectangles rest
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