module HilbertTree where
import Data.List
import Data.Ord
import Debug.Trace

-- XLow, XHigh, YLow, YHigh
data Rectangle = Rectangle Integer Integer Integer Integer deriving (Eq)
-- (MBR, Child, (minHilbert, maxHilbert))
type NodeData = (Rectangle, HilbertTree, (Integer, Integer))
data HilbertTree = Node [NodeData] | Leaf [Rectangle] deriving (Eq)


instance Show HilbertTree where
    show (Node nodes) = "N[" ++ concatMap (\(r,c,_) -> show (hilbert r) ++ ":" ++ show c ++ ",") nodes ++ "]"
    show (Leaf rects) = "L" ++ show rects


instance Show Rectangle where
    show r@(Rectangle xl xh yl yh) = "r " ++ show xl ++ " " ++ show xh ++ " " ++ show yl ++ " " ++ show yh ++ ":" ++ show (hilbert r)
--    show r = "r" ++ show(hilbert r)

r a b c d = Rectangle a b c d
----
tr :: [Rectangle]
tr = [Rectangle 0 1 0 1, Rectangle 1 2 1 2, Rectangle 3 5 3 5, Rectangle 0 4 0 4]

order tree = if (rall tree == sortBy (comparing hilbert) (rall tree)) then True else error ("not ordered\n" ++ show tree)

ok a = balanced a && allOk a && order a

allOk a = if allOk' a  then True else error "too big nodes"
allOk' (Node ns) = length (ns) <= maxNodeSize && all (\(_,c,_) -> allOk' c) ns
allOk' (Leaf rs) = length rs <= maxLeafSize 

balanced :: HilbertTree -> Bool 
balanced tree = if (1 >= length (group $  mapDepth 0 tree)) then True else error ("inbalanced" ++ show (tree))

mapDepth :: Int -> HilbertTree -> [Int]
mapDepth d (Node ns) = (\(_,c,_) -> mapDepth (d+1) c) =<< ns
mapDepth d (Leaf _) = [d]

rall (Node ns) = concatMap (\(_,c,_) -> rall c) ns
rall (Leaf rs) = rs

getNodes (Node ns) = ns
---- Constants

maxRectCoord = 66536

maxLeafSize :: Int
maxLeafSize = 2

maxNodeSize :: Int
maxNodeSize = 2

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
insertTree' (Node nodes) rect = (Node f1, grow best f2) where
    (f1, f2) = handleOverflow siblings (makeNode newTree) split
    siblings = getSiblings nodes b
    (newTree, split) = insertTree' best rect 
    b@(_,best,_) = findBestNode nodes (hilbert rect)
insertTree' leaf rect = (insertLeaf leaf rect, Nothing)

grow :: HilbertTree -> Maybe NodeData -> Maybe NodeData
grow _ x = x

-- Inserts the rectangle into the Leaf. May cause overflow which will be fixed later
insertLeaf :: HilbertTree -> Rectangle -> HilbertTree
insertLeaf (Leaf rects) rect = Leaf (sortBy (comparing hilbert) (rect:rects))

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

getChildren = concatMap (\(_,Node ns,_) -> ns) 

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
    allNodes = sortBy (compareNodeHilberts) (node:siblings)
    allRects = sortBy (comparing hilbert) (getAllRects allNodes)
    numRects = length allRects
    capacity = getCapacity allNodes
    newNode = last distributed
    oldNodes = init distributed
    distributed = (fill allRects)

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
getRects n@(Node _) = error ("getRects expects Leaf:" ++ show n)
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
getNodeHilbert (Node nodes) = (min, max)
    where min = minimum $ fmap (\(_,_,(shv,_)) -> shv) nodes
          max = maximum $ fmap (\(_,_,(_,lhv)) -> lhv) nodes
getNodeHilbert (Leaf rects) = (minimum hilberts, maximum hilberts)
    where hilberts = fmap hilbert rects

findBestNode (n@(_,_,(_,h)):n2:ns) hi | h > hi = n
                                      | otherwise = findBestNode (n2:ns) hi
findBestNode [n] _ = n


intersects :: Rectangle -> Rectangle -> Bool
intersects (Rectangle xl xh yl yh) (Rectangle xl2 xh2 yl2 yh2) = 
    not $ xl2 > xh || xh2 < xl || yh2 < yl || yl2 > yh

calculateMBR :: Rectangle -> Rectangle -> Rectangle
calculateMBR (Rectangle  xl xh yl yh)  (Rectangle xl2 xh2 yl2 yh2) = 
    Rectangle (min xl xl2) (max xh xh2) (min yl yl2) (max yh yh2)

hilbert :: Rectangle -> Integer
hilbert rect = hilbert' d x y where
    d = ceiling $ logBase 4 (maxRectCoord ** 2)
    (x, y) = center rect

-- I have no idea why (or if) this works
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
