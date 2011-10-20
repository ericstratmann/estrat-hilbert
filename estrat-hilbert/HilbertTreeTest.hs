import Test.QuickCheck
import Control.Monad
import HilbertTree
import Data.List
import Data.Ord


main = do
    q "Rect intersect self" p1
    q "Search tree" p2
    q "Tree is in order" p3
    q "Tree is balanced" p4
    q "Nodes/Leaves aren't too big" p5

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

p1 rect = intersects rect rect

p2 tree rect = rect `elem` searchTree (insertTree tree rect) rect

p3 tree = allRects tree == sortBy (comparing hilbert) (allRects tree)

p4 tree = 1 >= length (group $ mapDepth 0 tree)

p5 tree = checkSize tree

checkSize (Node ns) = length (ns) <= maxNodeSize && all (\(_,c,_) -> checkSize c) ns
checkSize (Leaf rs) = length rs <= maxLeafSize 

mapDepth :: Int -> HilbertTree -> [Int]
mapDepth d (Node ns) = (\(_,c,_) -> mapDepth (d+1) c) =<< ns
mapDepth d (Leaf _) = [d]


getNodes (Node ns) = ns
