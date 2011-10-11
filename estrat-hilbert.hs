import System.Environment

data Rectangle = Rectangle Integer Integer Integer Integer deriving Show

main :: IO ()
main = do
    fileRectangles <- readRectanglesFile
    inputRectangles <- readRectanglesStdin
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

replaceNewLines :: Char -> Char
replaceNewLines c | c == '\n' = ','
                  | otherwise = c

parseRectangles :: String -> [Rectangle]
parseRectangles rects = buildRectangles $ map read $ split ',' $ map replaceNewLines rects

buildRectangles :: [Integer] -> [Rectangle]
buildRectangles (w:x:y:z:rest) = (Rectangle w x y z) : buildRectangles rest
buildRectangles _ = []

split :: Char -> String -> [String]
split c s
  | findDelim == [] = []
  | otherwise = w : split c s'' where
      (w, s'') = break (c==) findDelim
      findDelim = dropWhile (c==) s
