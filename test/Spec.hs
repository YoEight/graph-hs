import Data.Graph

test_graph :: Graph IO Integer ()
test_graph = fromList [ (x, x) | x <- [1..10]] []

main :: IO ()
main = do
  xs <- collectVertices test_graph
  print xs
