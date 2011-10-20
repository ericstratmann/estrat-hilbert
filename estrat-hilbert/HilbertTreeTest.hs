import Test.QuickCheck
import HilbertTree
import Data.List
import Data.Ord


main :: IO ()
main = do
    q "Don't find bogus rectangle" p1
    q "Search tree" p2
    q "Tree is in order" p3
    q "Tree is balanced" p4
    q "Nodes/Leaves aren't too big" p5
    q "Find dups" p6
    q "MBR matches all in tree" p7

q :: Testable p => String -> p -> IO ()
q s p = putStrLn s >> quickCheck p

instance Arbitrary Rectangle where
    arbitrary = do
        x1 <- arbitrary
        x2 <- arbitrary
        y1 <- arbitrary
        y2 <- arbitrary
        let (x1', x2', y1', y2') = (abs x1, abs x2, abs y1, abs y2)
        let xLow = min x1' x2'
        let xHigh = max x1' x2'
        let yMin = min y1' y2'
        let yMax = max y1' y2'
        return $ Rectangle xLow xHigh yMin yMax


instance Arbitrary HilbertTree where
    arbitrary = do
        rects <- listOf arbitrary
        return $ buildTree rects

p1 :: HilbertTree -> Bool
p1 tree = 0 == length (searchTree tree (Rectangle (-2) (-1) (-2) (-1)))

p2 :: HilbertTree -> Rectangle -> Bool
p2 tree rect = rect `elem` searchTree (insertTree tree rect) rect

p3 :: HilbertTree -> Bool
p3 tree = allRects tree == sortBy (comparing hilbert) (allRects tree)

p4 :: HilbertTree -> Bool
p4 tree = 1 >= length (group $ mapDepth 0 tree)

p5 :: HilbertTree -> Bool
p5 = checkSize

p6 :: HilbertTree -> Rectangle -> Bool
p6 tree rect = 2 <= length (searchTree newTree rect)
    where newTree = insertTree (insertTree tree rect) rect

p7 :: HilbertTree -> [Rectangle] -> Bool
p7 tree rects = and $ map (`elem` results) rects
    where
    results = searchTree newTree mbr
    mbr = foldl1' calculateMBR rects
    newTree = foldl' insertTree tree rects

checkSize :: HilbertTree -> Bool
checkSize (Node ns) = length ns <= maxNodeSize && all (\(_,c,_) -> checkSize c) ns
checkSize (Leaf rs) = length rs <= maxLeafSize 

mapDepth :: Int -> HilbertTree -> [Int]
mapDepth d (Node ns) = (\(_,c,_) -> mapDepth (d+1) c) =<< ns
mapDepth d (Leaf _) = [d]
