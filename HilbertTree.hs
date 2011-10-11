module HilbertTree where
import Data.List

data Rectangle = Rectangle Integer Integer Integer Integer deriving (Show, Eq)
data Point = Point Integer Integer deriving (Show, Eq)
type NodeData = (Rectangle, HilbertTree, Integer)
data HilbertTree = Node [(NodeData)] | Leaf [Rectangle] deriving (Show, Eq)

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

-- placeholder definition
hilbert :: Rectangle -> Integer
hilbert rect = div (x + y) 2 where
    (Point x y) = center rect

center :: Rectangle -> Point
center (Rectangle xl xh yl yh) = Point (div (xh+xl) 2) (div (yh+yl) 2)
