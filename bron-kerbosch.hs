import Data.List
import Data.Ord

type Graph = [(Int, [Int])]

adjacentVertices :: Graph -> (Int, [Int]) -> Graph
adjacentVertices [] _ = []
adjacentVertices g v = [x | x <- g, (fst v) `elem` (snd x)]

maxCliques :: Graph -> Graph -> Graph -> [Graph] -> [Graph]
maxCliques r [] [] s = r:s
maxCliques _ [] _ s = s
maxCliques r (p:ps) x s = (maxCliques r ps (p:x) (maxCliques (p:r) (adjacentVertices (p:ps) p) (adjacentVertices x p) s))

showClique [] = []
showClique s = sort [fst x | x <- s]

functionCliques g = [showClique s | s <- (maxCliques [] g [] [])]
