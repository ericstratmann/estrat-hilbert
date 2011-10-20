module HilbertTree where
import Data.List
import Data.Ord

-- XLow, XHigh, YLow, YHigh
data Rectangle = Rectangle Integer Integer Integer Integer deriving (Show, Eq)
-- (MBR, Child, (minHilbert, maxHilbert))
type NodeData = (Rectangle, HilbertTree, (Integer, Integer))
data HilbertTree = Node [NodeData] | Leaf [Rectangle] deriving (Show, Eq)

---- Constants

maxRectCoord = 66536

maxLeafSize :: Int
maxLeafSize = 3

maxNodeSize :: Int
maxNodeSize = 3

---- Public functions

emptyTree :: HilbertTree
emptyTree = Node []

buildTree :: [Rectangle] -> HilbertTree
buildTree =  foldl' insertTree emptyTree

insertTree :: HilbertTree -> Rectangle -> HilbertTree
insertTree tree rect = a
    where
    a = newRoot newTree split
    (newTree, split) = insertTree' tree rect
    
searchTree :: HilbertTree -> Rectangle -> [Rectangle]
searchTree (Node nodes) rect = searchTree' =<< nodes where
    searchTree' (rect2, tree, _) | intersects rect rect2 = searchTree tree rect
                                   | otherwise = []
searchTree (Leaf leafs) rect = searchTree'' =<< leafs where
    searchTree'' rect2 | intersects rect rect2 = [rect2]
                       | otherwise = []

---- Private functions
    
newRoot :: HilbertTree -> Maybe NodeData -> HilbertTree
newRoot tree (Just newNode) = Node $ sortBy compareNodeHilberts [makeNode tree, makeNode $ Node [newNode]]
newRoot tree Nothing = tree

insertTree' :: HilbertTree -> Rectangle -> (HilbertTree, Maybe NodeData)
insertTree' (Node []) rect = (Node [(rect, Leaf [rect], (hilbert rect, hilbert rect))], Nothing)
insertTree' (Node nodes) rect = (Node f1, f2) where
    (f1, f2) = handleOverflow siblings (makeNode newTree) split
    siblings = getSiblings nodes b
    (newTree, split) = insertTree' best rect 
    b@(_,best,_) = findBestNode nodes (hilbert rect)
insertTree' leaf rect = (insertLeaf leaf rect, Nothing)

-- Inserts the rectangle into the Leaf. May cause overflow which will be fixed later
insertLeaf :: HilbertTree -> Rectangle -> HilbertTree
insertLeaf (Leaf rects) rect = Leaf (sortBy (comparing hilbert) (rect:rects))
insertLeaf _ _ = error "insertLeaf expects leaf"

--fix (a,b) c = trace (s "vvvvv" ++ s a ++ s b ++ s c ++ s "^^^^^^^") (fix' (a,b) c)

getRectCount :: HilbertTree -> Int
getRectCount (Node nodes) = sum $ fmap (\(_,t,_) -> getRectCount t) nodes
getRectCount (Leaf rects) = length rects
           
isOverflow :: HilbertTree -> Bool
isOverflow (Node nodes) = length nodes > maxNodeSize
isOverflow (Leaf rects) = length rects > maxLeafSize

handleOverflow :: [NodeData] -> NodeData -> Maybe NodeData -> ([NodeData], Maybe NodeData)
handleOverflow siblings node split@(Just _) = handleOverflowNode split (node:siblings)
handleOverflow siblings node Nothing | isLeafNode node = handleOverflowLeaf node siblings
                                     | otherwise = handleOverflowNode Nothing (node:siblings)

handleOverflowNode :: Maybe NodeData -> [NodeData] -> ([NodeData], Maybe NodeData)
handleOverflowNode Nothing siblings = (sorted, Nothing)
    where sorted = sortBy compareNodeHilberts siblings
handleOverflowNode (Just newNode) nodes | length allNodes  <= capacity = (distributed, Nothing)
                                        | otherwise = (first, new)
    where
    capacity = length nodes * maxNodeSize
    first = init distributed
    new = Just (last distributed)
    allNodes = newNode : getChildren nodes
    sorted = sortBy compareNodeHilberts allNodes
    distributed = fillD sorted

getChildren :: [NodeData] -> [NodeData]
getChildren = (=<<) (\(_,Node ns,_) -> ns) 

fillD :: [NodeData] -> [NodeData]
fillD nodes | not $ null nodes = (makeNode . Node $ take maxNodeSize nodes) : fillD (drop maxLeafSize nodes)
fillD _ = []


getRect :: NodeData -> Rectangle
getRect (rect,_,_) = rect

handleOverflowLeaf :: NodeData -> [NodeData] -> ([NodeData], Maybe NodeData)
handleOverflowLeaf node siblings | not full = (allNodes, Nothing)
                                 | numRects <= capacity = (distributed, Nothing)
                                 | otherwise = (oldNodes, Just newNode)
    where
    full = any (\(_,Leaf rs,_) -> length rs > maxLeafSize) allNodes
    allNodes = sortBy compareNodeHilberts (node:siblings)
    rects = sortBy (comparing hilbert) (getAllRects allNodes)
    numRects = length rects
    capacity = getCapacity allNodes
    newNode = last distributed
    oldNodes = init distributed
    distributed = fill rects

compareNodeHilberts :: NodeData -> NodeData -> Ordering
compareNodeHilberts (_,_,h1) (_,_,h2) = compareHilberts h1 h2

compareHilberts :: (Integer, Integer) -> (Integer, Integer) -> Ordering 
compareHilberts (shv1, lhv1) (shv2, lhv2) | lhv1 == lhv2 && shv1 == shv2 = EQ
                                          | lhv1 < lhv2 = LT
                                          | lhv1 > lhv2 = GT
                                          | shv1 < shv2 = LT
                                          | otherwise = GT

makeNode :: HilbertTree -> NodeData
makeNode tree = fixNode (undefined, tree, undefined)

fill :: [Rectangle] -> [NodeData]
fill rects | not $ null rects = (makeNode . Leaf $ take maxLeafSize rects) : fill (drop maxLeafSize rects)
fill _ = []

update :: [NodeData] -> NodeData -> HilbertTree -> [NodeData]
update (n:ns) node@(rect, _, (shv, lhv)) tree | n == node = (rect, tree, (shv, lhv)) : update ns node tree
                                              | otherwise = node : update ns node tree 
update [] _ _ = []

getSiblings :: [NodeData] -> NodeData -> [NodeData]
getSiblings (n:ns) node | n == node = getSiblings ns node
                        | otherwise = n : getSiblings ns node
getSiblings _ _ = []

isLeaf :: HilbertTree -> Bool
isLeaf (Leaf _) = True
isLeaf (Node _) = False


isLeafNode :: NodeData -> Bool
isLeafNode (_,child,_) | isLeaf child = True
                       | otherwise = False

-- children must be leaf nodes
getAllRects :: [NodeData] -> [Rectangle]
getAllRects = (=<<) (\(_,c,_) -> getRects c) 
getRects :: HilbertTree -> [Rectangle]
getRects (Node _) = error "getRects expects Leaf"
getRects (Leaf rects) = rects

-- children must be leaf nodes
getCapacity :: [NodeData] -> Int
getCapacity nodes = length nodes * maxLeafSize


fixNode :: NodeData -> NodeData
fixNode (_, child, _) = (mbr, child, hilberts) where
    mbr = getNodeMBR child
    hilberts = getNodeHilbert child

getNodeMBR :: HilbertTree -> Rectangle
getNodeMBR (Node nodes) = foldl1 calculateMBR $ fmap (\(mbr,_,_) -> mbr) nodes
getNodeMBR (Leaf rects) = foldl1 calculateMBR rects

getNodeHilbert :: HilbertTree -> (Integer, Integer)
getNodeHilbert (Node nodes) = (minH, maxH)
    where minH = minimum $ fmap (\(_,_,(shv,_)) -> shv) nodes
          maxH = maximum $ fmap (\(_,_,(_,lhv)) -> lhv) nodes
getNodeHilbert (Leaf rects) = (minimum hilberts, maximum hilberts)
    where hilberts = fmap hilbert rects

findBestNode :: [NodeData] -> Integer -> NodeData
findBestNode (n@(_,_,(_,h)):n2:ns) hi | h > hi = n
                                      | otherwise = findBestNode (n2:ns) hi
findBestNode [n] _ = n
findBestNode _ _ = error "findBestNode expects node"

intersects :: Rectangle -> Rectangle -> Bool
intersects (Rectangle xl xh yl yh) (Rectangle xl2 xh2 yl2 yh2) = 
    not $ xl2 > xh || xh2 < xl || yh2 < yl || yl2 > yh

calculateMBR :: Rectangle -> Rectangle -> Rectangle
calculateMBR (Rectangle  xl xh yl yh)  (Rectangle xl2 xh2 yl2 yh2) = 
    Rectangle (min xl xl2) (max xh xh2) (min yl yl2) (max yh yh2)

allRects :: HilbertTree -> [Rectangle]
allRects (Node ns) = (\(_,c,_) -> allRects c) =<< ns
allRects (Leaf rs) = rs

hilbert :: Rectangle -> Integer
hilbert rect = hilbert' d x y where
    d = ceiling $ logBase 4 (maxRectCoord ** 2)
    (x, y) = center rect

-- Adapted from http://www.serpentine.com/blog/2007/01/11/two-dimensional-spatial-hashing-with-space-filling-curves/
hilbert' :: Integer -> Integer -> Integer -> Integer
hilbert' d x y = dist (2^(d-1)) (2^(2*(d-1))) 0 x y where
    dist 0 _ result _ _ = result
    dist side area result x' y'
        | x' < side && y' < side = dist newDist newArea result y' x'
        | x' < side = dist newDist newArea (result + area) x' (y' - side)
        | y' < side = dist newDist newArea (result + area * 3) (side - y' - 1) (side * 2 - x' -1)
        | otherwise = dist newDist newArea (result + area * 2) (x' - side) (y' - side)
        where newDist = div side 2
              newArea = div area 4
        
center :: Rectangle -> (Integer, Integer)
center (Rectangle xl xh yl yh) = (div (xh+xl) 2, div (yh+yl) 2)
