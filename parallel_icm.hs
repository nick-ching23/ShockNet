import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Data.List (nub)
import Control.Monad.Random
import Control.Parallel.Strategies
import GHC.Conc (getNumCapabilities)

type SimpleEdge = (Int, Int)

generateEdges :: Int -> IO [SimpleEdge]
generateEdges n = replicateM n $ do
    i <- randomRIO (0, 999)
    j <- randomRIO (0, 999)
    if i /= j
        then return (i, j)
        else do
            let loop = do
                    j' <- randomRIO (0, 999)
                    if i /= j' then return (i, j') else loop
            loop

generateUniqueEdges :: Int -> IO [SimpleEdge]
generateUniqueEdges n = do
    edges <- generateEdges (n * 2)  -- Generate extra edges to account for duplicates
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
    edges <- generateWeightedEdges 9990  -- Approximate expected number of edges
    return $ mkGraph nodes edges



-- THE ACTUAL PARALLELIZATION OF THE MODEL

independentCascade :: Gr () Double -> [Node] -> Rand StdGen (Set.Set Node)
independentCascade graph seeds = go (Set.fromList seeds) (Set.fromList seeds)
  where
    go :: Set.Set Node -> Set.Set Node -> Rand StdGen (Set.Set Node)
    go activatedNodes newlyActivated
        | Set.null newlyActivated = return activatedNodes
        | otherwise = do
            nextActivatedList <- forM (Set.toList newlyActivated) $ \node -> do
                let neighbors = lsuc graph node  -- Get successors with edge weights
                activatedNeighbors <- forM neighbors $ \(neighbor, weight) -> do
                    if neighbor `Set.member` activatedNodes
                        then return Nothing
                        else do
                            r <- getRandomR (0.0, 1.0 :: Double)
                            if r <= weight
                                then return $ Just neighbor
                                else return Nothing
                return $ catMaybes activatedNeighbors
            let nextActivated = Set.fromList $ concat nextActivatedList
            let activatedNodes' = Set.union activatedNodes nextActivated
            go activatedNodes' nextActivated

-- Optimization is to chunk
monteCarloSimulation :: Gr () Double -> [Node] -> Int -> IO Double
monteCarloSimulation graph seeds numSimulations = do
    numCapabilities <- getNumCapabilities
    stdGen <- getStdGen
    let gens = take numSimulations $ iterate (snd . split) stdGen 
    let chunkSize = (numSimulations + numCapabilities - 1) `div` numCapabilities
    let chunks = chunkList chunkSize gens 
    let results = parMap rdeepseq (\genChunk ->
                    sum [ evalRand (simulateOnce graph seeds) gen | gen <- genChunk ]
                  ) chunks
    let totalActivated = sum results
    let averageInfluence = totalActivated / fromIntegral numSimulations
    return averageInfluence
  where
    simulateOnce :: Gr () Double -> [Node] -> Rand StdGen Double
    simulateOnce g s = do
        activatedNodes <- independentCascade g s
        return $ fromIntegral $ Set.size activatedNodes

    chunkList :: Int -> [a] -> [[a]]
    chunkList _ [] = []
    chunkList n xs = take n xs : chunkList n (drop n xs)

main :: IO ()
main = do
    graph <- buildGraph
    let seedNodes = [0]           
    let numSimulations = 1000 
    averageInfluence <- monteCarloSimulation graph seedNodes numSimulations
    putStrLn $ "Average Influence (Monte Carlo): " ++ show averageInfluence