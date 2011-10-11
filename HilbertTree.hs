module HilbertTree where
import Data.List

data Rectangle = Rectangle Integer Integer Integer Integer deriving (Show, Eq)
data Point = Point Integer Integer deriving (Show, Eq)
type NodeData = (Rectangle, HilbertTree, Integer)
data HilbertTree = Node [(NodeData)] | Leaf [Rectangle] deriving (Show, Eq)

maxRectCoord = 66536

emptyTree :: HilbertTree
emptyTree = Node []

buildTree :: [Rectangle] -> HilbertTree
buildTree rects =  foldl' insertTree (Leaf []) []

insertTree :: HilbertTree -> Rectangle -> HilbertTree
insertTree (Node []) rect = Node [(rect, Leaf [rect], hilbert rect)]
insertTree (Node nodes) rect = Node (update nodes b (insertTree best rect))
    where b@(_,best,_) = findBestNode nodes rect
insertTree (Leaf rects) rect = Leaf (rect:rects)

update :: [NodeData] -> NodeData -> HilbertTree -> [NodeData]
update (n:ns) node@(rect, _, lhv) tree | n == node = (rect, tree, lhv) : update ns node tree
                                       | otherwise = node : update ns node tree
update [] _ _ = []

findBestNode :: [(NodeData)] -> Rectangle -> (NodeData)
findBestNode nodes rect = maximumBy getBest nodes where
    rHilbert = hilbert rect
    getBest (_,_,lhv) (_,_,lhv2) | lhv < rHilbert = LT
                                 | lhv < lhv2 = GT
                                 | otherwise = LT

searchTree :: HilbertTree -> Rectangle -> [Rectangle]
searchTree (Node nodes) rect = concatMap searchTree' nodes where
    searchTree' (rect2, tree, lhv) | intersects rect rect2 = searchTree tree rect
                                   | otherwise = []
searchTree (Leaf leafs) rect = concatMap searchTree'' leafs where
    searchTree'' rect2 | intersects rect rect2 = [rect2]
                       | otherwise = []


intersects (Rectangle xl xh yl yh) (Rectangle xl2 xh2 yl2 yh2) = 
    not $ xl2 > xh || xh2 < xl || yh2 > yl || yl2 < yh

getMBR :: [Rectangle] -> Rectangle
getMBR rectangles = getMBR' (head rectangles) (tail rectangles) where
        getMBR' mbr (rect:rest) = getMBR' (generateMBR mbr rect) rest

generateMBR :: Rectangle -> Rectangle -> Rectangle
generateMBR (Rectangle  xl xh yl yh)  (Rectangle xl2 xh2 yl2 yh2) = 
    Rectangle (min xl xl2) (max xh xh2) (min xl xl2) (max xh xh2)

hilbert :: Rectangle -> Integer
hilbert rect = hilbert' d x y where
    d = ceiling $ logBase 4 (maxRectCoord ** 2)
    Point x y = center rect

-- I have no idea why (or if) this works
hilbert' :: Integer -> Integer -> Integer -> Integer
hilbert' d x y = dist (2^(d-1)) (2^(2*(d-1))) 0 x y where
    dist 0 _ result _ _ = result
    dist side area result x y
        | x < side && y < side = dist newDist newArea result y x
        | x < side = dist newDist newArea (result + area) x (y - side)
        | y < side = dist newDist newArea (result + area * 3) (side - y - 1) (side * 2 - x -1)
        | otherwise = dist newDist newArea (result + area * 2) (x - side) (y - side)
        where newDist = div side 2
              newArea = div area 4
        

center :: Rectangle -> Point
center (Rectangle xl xh yl yh) = Point (div (xh+xl) 2) (div (yh+yl) 2)
