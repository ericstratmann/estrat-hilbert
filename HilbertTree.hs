module HilbertTree where
import Data.List

data Rectangle = Rectangle Integer Integer Integer Integer deriving Show
data HilbertTree = Node HilbertTree HilbertTree Int | EmptyTree

buildTree :: [Rectangle] -> HilbertTree
buildTree rects =  foldl' insertTree EmptyTree []

insertTree :: HilbertTree -> Rectangle -> HilbertTree
insertTree tree rect = tree

searchTree :: HilbertTree -> Rectangle -> [Rectangle]
searchTree tree rect = []
