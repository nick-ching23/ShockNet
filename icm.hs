import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Data.List (nub)

type SimpleEdge = (Int, Int)

generateEdges :: Int -> IO [SimpleEdge]
generateEdges n = replicateM n $ do
    i <- randomRIO (0, 999)
    j <- randomRIO (0, 999)
    if i /= j
        then return (i, j)
        else do
            -- Retry if i == j to avoid self-loops
            let loop = do
                    j' <- randomRIO (0, 999)
                    if i /= j' then return (i, j') else loop
            loop

generateUniqueEdges :: Int -> IO [SimpleEdge]
generateUniqueEdges n = do
    edges <- generateEdges (n * 2)
    return $ take n $ nub edges

generateWeightedEdges :: Int -> IO [LEdge Double]
generateWeightedEdges n = do
    edges <- generateUniqueEdges n
    forM edges $ \(i, j) -> do
        weight <- randomRIO (0.1, 0.5)
        return (i, j, weight)

buildGraph :: IO (Gr () Double)
buildGraph = do
    let nodes = [(i, ()) | i <- [0..999]]
    edges <- generateWeightedEdges 9990 
    return $ mkGraph nodes edges

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

monteCarloSimulation :: Gr () Double -> [Node] -> Int -> IO Double
monteCarloSimulation graph seeds numSimulations = do
    totalActivated <- replicateM numSimulations $ do
        activatedNodes <- independentCascade graph seeds
        return $ fromIntegral $ Set.size activatedNodes
    let total = sum totalActivated
    return $ total / fromIntegral numSimulations

-- Main function to run the simulation and print the average influence
main :: IO ()
main = do
    graph <- buildGraph
    let seedNodes = [0]          
    let numSimulations = 1000    
    averageInfluence <- monteCarloSimulation graph seedNodes numSimulations
    putStrLn $ "Average Influence (Monte Carlo): " ++ show averageInfluence