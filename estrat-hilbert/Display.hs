import Graphics.Gloss
import HilbertTree hiding (Point)

main = do
    let (rects, mbr) = getTree
    let pics = drawTree mbr rects
    displayInWindow "My Window" (wSize+100, wSize+100) (10, 10) white (Pictures pics)


wSize = 500 :: Int


getTree :: (HilbertTree, Rectangle)
getTree = (tree, mbr)
    where rects = [Rectangle 1 3 1 3, Rectangle 2 4 2 4,
                  Rectangle 8 16 8 16, Rectangle 6 20 4 25]
          tree = buildTree rects
          mbr = getMBR rects



drawTree :: Rectangle -> HilbertTree -> [Picture]
drawTree mbr (Node nodes) = drawNode mbr <<= nodes
drawTree mbr (Leaf rects) =  fmap (rectToPic mbr) rects

drawNode :: Rectangle -> NodeData -> [Picture]
drawNode tMBR (mbr,child,_) = Color red (rectToPic tMBR mbr) : drawTree tMBR child

drawLeaf :: Rectangle -> [Rectangle] -> [Picture]
drawLeaf mbr = fmap (rectToPic mbr)


rectToPic :: Rectangle -> Rectangle -> Picture
rectToPic mbr (Rectangle xl xh yl yh) = Line $ fmap (toF mbr)
    [
        (xl,yl), (xl,yh), (xh,yh), (xh,yl),(xl,yl)
    ]


toF (Rectangle _ mxh _ myh) (x, y) =
    (fi wSize * fi (x-5)/fi (mxh +5),
    fi wSize * fi (y-5))/fi (myh + 5)

fi = fromIntegral
