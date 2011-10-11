import Test.QuickCheck
import Control.Monad
import HilbertTree

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

prop_same_rects_intersect rect = intersects rect rect

prop_rect_search tree rect = rect `elem` searchTree (insertTree tree rect) rect
