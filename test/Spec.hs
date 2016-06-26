import Data.List

import Data.Conduit
import Test.Tasty
import Test.Tasty.Ingredients.Basic
import Test.Tasty.HUnit

import Data.Graph

tests :: TestTree
tests = testGroup "Graph tests"
    [ testCase "Simple graph" testSimpleGraph
    , testCase "Join graphs" testJoinGraph
    , testCase "Connected Components" testConnectedComponents
    , testCase "Degrees" testDegrees
    ]

testSimpleGraph :: IO ()
testSimpleGraph = do

    let xs    = [ (x, x) | x <- [1..10] ]
        graph = fromList xs []

    vs <- collectVertices graph
    assertEqual "Vertices should be equal" xs vs

testJoinGraph :: IO ()
testJoinGraph = do

    let xs       = [ (x, x) | x <- [1..3] ]
        expected = [ (x, (x, x)) | x <- [1..3] ]
        graph_1  = fromList xs []
        graph_2  = fromList (reverse xs) []
        src      = join (vertices graph_1) (vertices graph_2)

    fin <- sourceToList src
    assertEqual "Vertices should be equal" fin expected

testConnectedComponents :: IO ()
testConnectedComponents = do

    let vs = [ (x, x) | x <- [1..6] ]
        es = [ (1, 3, ())
             , (1, 5, ())
             , (3, 5, ())
             , (2, 4, ())
             , (2, 6, ())
             , (4, 6, ())
             ]
        expected = sort [ (1, 1)
                        , (3, 1)
                        , (5, 1)
                        , (2, 2)
                        , (4, 2)
                        , (6, 2)
                        ]
        graph = fromList vs es
        cc    = connectedComponents graph

    fin <- collectVertices cc
    assertEqual "Vertices should be equal" fin expected

testDegrees :: IO ()
testDegrees = do

    let vs = [ (x, x) | x <- [1..7] ]
        es = [ (2, 3, ())
             , (3, 4, ())
             , (3, 5, ())
             , (4, 7, ())
             , (5, 6, ())
             , (5, 7, ())
             , (6, 7, ())
             , (7, 7, ())
             ]
        expected = [ (2, 1)
                   , (3, 3)
                   , (4, 2)
                   , (5, 3)
                   , (6, 2)
                   , (7, 5)
                   ]
        graph = fromList vs es
        dg    = degrees graph

    fin <- collectVertices dg
    assertEqual "Vertices should be equal" fin expected

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] tests
