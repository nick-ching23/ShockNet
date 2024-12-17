import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Data.List (foldl')

type SimpleEdge = (Int, Int)


-- TESTING PARAMETERS -- 

nodesCount :: Int
nodesCount = 30000

edgesCount :: Int
edgesCount = 300000

numSimulations :: Int
numSimulations = 1


-- MAIN METHOD -- 

{-|
  Main method:
    - builds the graph with specified nodesCount & edgesCount 
    - executes numSimulations number of monte carlo simulations of the ICM using the seed 
       set (in this case node 0)
    - displays resulting average influence
-}
main :: IO ()
main = do
    putStrLn "Building graph..."
    graph <- buildGraph
    putStrLn "Running Monte Carlo simulations..."
    let seedNodes = [0]
    averageInfluence <- monteCarloSimulation graph seedNodes numSimulations
    putStrLn $ "Average Influence (Monte Carlo): " ++ show averageInfluence


-- METHODS FOR BUILDING THE GRAPH -- 
 
{-|
    Builds a list of unique edges (without repition or self-looping)
    Basically, we pick any random two nodes, and if no edge exists between them,
    we add the new edge to a set. 
-}

generateUniqueEdges :: Int -> IO [SimpleEdge]
generateUniqueEdges n = do
    let loop acc s
          | Set.size s >= n = return (Set.toList s)
          | otherwise = do
              i <- randomRIO (0, nodesCount-1)
              j <- randomRIO (0, nodesCount-1)
              if i /= j then 
                  let s' = Set.insert (i,j) s
                  in loop acc s'
              else loop acc s
    loop [] Set.empty


{-|
    This method assigns weights to each edge. We pick the weights for each edge
    picking a random float between [0.1, 0.5] 
-}

generateWeightedEdges :: Int -> IO [LEdge Double]
generateWeightedEdges n = do
    edges <- generateUniqueEdges n
    forM edges $ \(i, j) -> do
        weight <- randomRIO (0.1, 0.5)
        return (i, j, weight)

{-|
  Build a directed graph with a specified number of nodes and a specified number 
  of randomly generated edges. 
-}

buildGraph :: IO (Gr () Double)
buildGraph = do
    let nodes = [(i, ()) | i <- [0..nodesCount-1]]
    edges <- generateWeightedEdges edgesCount
    return $ mkGraph nodes edges


-- INDEPENDENT CASCADE AND MONTE CARLO SIMULATIONS 

{-|
  Perform one run of ICM given a graph and a set of initially activated nodes (seeds). 
  Refer to the Problem formulation in our report for an explanation of the ICM model    
-}   
-- note here we aren't using stdGen
independentCascade :: Gr () Double -> [Node] -> IO (Set.Set Node) 
independentCascade graph seeds = go (Set.fromList seeds) (Set.fromList seeds)
  where 
    go :: Set.Set Node -> Set.Set Node -> IO (Set.Set Node)
    go activatedNodes newlyActivated
        | Set.null newlyActivated = return activatedNodes
        | otherwise = do
            nextActivatedList <- forM (Set.toList newlyActivated) $ \node -> do
                let neighbors = lsuc graph node
                activatedNeighbors <- forM neighbors $ \(neighbor, weight) -> do   
                    if neighbor `Set.member` activatedNodes
                        then return Nothing
                        else do
                            r <- randomRIO (0.0, 1.0 :: Double) 
                            if r <= weight
                                then return $ Just neighbor  
                                else return Nothing
                return $ catMaybes activatedNeighbors
            let nextActivated = Set.fromList $ concat nextActivatedList
            let activatedNodes' = Set.union activatedNodes nextActivated  
            go activatedNodes' nextActivated
 

{-|
  Perform a Monte Carlo simulations

  The simulation repeats the Independent Cascade process a specified number of times 
  (`numSimulations`). This function returns the average influence over each simulation.

  This version uses replicateM to run the simulation multiple times 
  sequentially and accumulate the results.
-}
monteCarloSimulation :: Gr () Double -> [Node] -> Int -> IO Double
monteCarloSimulation graph seeds numSimulations = do
    totalActivated <- replicateM numSimulations $ do  
        activatedNodes <- independentCascade graph seeds 
        return $ fromIntegral $ Set.size activatedNodes
    let total = sum totalActivated 
    return $ total / fromIntegral numSimulations






