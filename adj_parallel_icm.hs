import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Control.Monad.Random
import Data.List (foldl')
import Control.Parallel.Strategies
import GHC.Conc (getNumCapabilities)
import qualified Data.Vector as V  -- For adjacency storage

type SimpleEdge = (Int, Int)

-- TESTING PARAMETERS --
nodesCount :: Int
nodesCount = 30000

edgesCount :: Int
edgesCount = 300000

numSimulations :: Int
numSimulations = 1000

main :: IO ()
main = do
    putStrLn "Building graph..."
    graph <- buildGraph
   
    -- Build an adjacency list once!!! This means we don't have to constantly access and allocate memory!!!
    let adjList = V.generate nodesCount (\node -> lsuc graph node)
    putStrLn "Graph built. Running Monte Carlo simulations..."
    let seedNodes = [0]
    averageInfluence <- monteCarloSimulation graph adjList seedNodes numSimulations
    putStrLn $ "Average Influence (Monte Carlo): " ++ show averageInfluence

-- METHODS FOR BUILDING THE GRAPH --

generateUniqueEdges :: Int -> IO [SimpleEdge]
generateUniqueEdges n = do
    let loop s
          | Set.size s >= n = return (Set.toList s)
          | otherwise = do
              i <- randomRIO (0, nodesCount-1)
              j <- randomRIO (0, nodesCount-1)
              if i /= j
                  then let s' = Set.insert (i,j) s
                       in loop s'
                  else loop s
    loop Set.empty

generateWeightedEdges :: Int -> IO [LEdge Double]
generateWeightedEdges n = do
    edges <- generateUniqueEdges n
    forM edges $ \(i, j) -> do
        weight <- randomRIO (0.1, 0.5)
        return (i, j, weight)

buildGraph :: IO (Gr () Double)
buildGraph = do
    let nodes = [(i, ()) | i <- [0..nodesCount-1]]
    edges <- generateWeightedEdges edgesCount
    return $ mkGraph nodes edges

-- INDEPENDENT CASCADE AND MONTE CARLO SIMULATIONS

-- Instead of passing the graph alone, we now also pass the adjacency list!!!
independentCascade :: V.Vector [(Node, Double)] -> [Node] -> Rand StdGen (Set.Set Node)
independentCascade adjList seeds = go (Set.fromList seeds) (Set.fromList seeds)
  where
    go :: Set.Set Node -> Set.Set Node -> Rand StdGen (Set.Set Node)
    go activatedNodes newlyActivated
        | Set.null newlyActivated = return activatedNodes
        | otherwise = do
            nextActivatedList <- forM (Set.toList newlyActivated) $ \node -> do
                let neighbors = adjList V.! node 
                activatedNeighbors <- forM neighbors $ \(neighbor, weight) ->
                    if neighbor `Set.member` activatedNodes
                        then return Nothing
                        else do
                            r <- getRandomR (0.0, 1.0 :: Double)
                            return $ if r 
                return $ catMaybes activatedNeighbors
            let nextActivated = Set.fromList $ concat nextActivatedList
            let activatedNodes' = Set.union activatedNodes nextActivated
            go activatedNodes' nextActivated

monteCarloSimulation :: Gr () Double -> V.Vector [(Node, Double)] -> [Node] -> Int -> IO Double
monteCarloSimulation graph adjList seeds numSims = do
    numCapabilities <- getNumCapabilities
    stdGen <- getStdGen
    let gens = take numSims $ iterate (snd . split) stdGen
    let chunkSize = (numSims + numCapabilities - 1) `div` numCapabilities
    let chunks = chunkList chunkSize gens
    let results = parMap rdeepseq (\genChunk ->
                    sum [ evalRand (simulateOnce adjList seeds) gen | gen <- genChunk ]
                  ) chunks

    let totalActivated = sum results
    let averageInfluence = totalActivated / fromIntegral numSims
    return averageInfluence
  where
    simulateOnce :: V.Vector [(Node, Double)] -> [Node] -> Rand StdGen Double
    simulateOnce adj s = do
        activatedNodes <- independentCascade adj s
        return $ fromIntegral $ Set.size activatedNodes

    chunkList :: Int -> [a] -> [[a]]
    chunkList _ [] = []
    chunkList n xs = take n xs : chunkList n (drop n xs)
