{-# LANGUAGE FlexibleContexts #-}
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)
import Data.Dequeue as S

data Color = W | G | B deriving (Show) -- node colors, i,e, W - white, G - grey, B - black

instance Eq Color where
  W == W = True
  G == G = True
  B == B = True
  _ == _ = False

-- Psuedocode as implemented in the Koran of Algorithms, also called "Introduction to Algorithms" by CRLS

bfs :: (Ix v) => v -> v -> Array v [v] -> (Array v v, Array v Color, Array v Int)  -- BFS(G,v) takes a graph with a source vertex, and returns an array of predecessors. Starting from src, you can retrieve the traveral
bfs src invalid_index adj_list = runST $ do
  color <- newSTArray b W
  writeArray color src G -- Initialise src color to gray
  dist <- newSTArray b maxBound
  writeArray dist src 0
  prev <- newSTArray b invalid_index
  let aux q =
        case S.null q of
          True -> return ()
          False ->
            let Just (u, q') = S.popFront q
                edges = adj_list ! u
                f q' v = do
                  x <- readArray color v
                  if ( x == W ) then do
                   writeArray color v G
                   dist_u <- readArray dist u
                   writeArray dist v (dist_u + 1)
                   writeArray prev v u
                   return $ S.pushBack q' v
                  else
                   return q'
            in
            foldM f q' edges >>= aux >> writeArray color u B
  let q = S.empty :: S.BankersDequeue v
  aux (S.pushBack q src)
  p <- freeze prev
  c <- freeze color
  d <- freeze dist
  return (p, c, d)
  where b = bounds adj_list
        newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newSTArray = newArray

adj_list :: Array Char [Char]
adj_list = listArray ('a', 'h') [ ['b', 'f'],
                                  ['a','e'],
                                  ['d', 'f','g'],
                                  ['c', 'g', 'h'],
                                  ['b'],
                                  ['a', 'c', 'g'],
                                  ['c', 'd', 'f', 'h'],
                                  ['d', 'g'] ] 

main :: IO ()
main = do
         let (prev, color, dist) = bfs 'a' ' ' adj_list
