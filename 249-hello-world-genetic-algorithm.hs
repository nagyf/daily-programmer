import Control.Monad.Writer
import System.Random
import Data.Char(chr)
import Data.List(minimumBy)
import Data.Function(on)

-- https://www.reddit.com/r/dailyprogrammer/comments/40rs67/20160113_challenge_249_intermediate_hello_world/

main :: IO ()
main = do
    input <- getLine
    p <- population 1024 (length input)
    -- Run the genetic algorithm
    (result, steps) <- runWriterT (algorithm input p)
    -- Print the result
    mapM_ putStrLn $ zipWith display [1..] steps

    where
        display :: Int -> String -> String
        display n result = "Gen: " ++ show n ++ " | " ++ result ++ " | "

-- The genetic algorithm
algorithm :: String -> [String] -> WriterT [String] IO String
algorithm input population = do
    let (best, bestFitness) = fittest input population
    let rem = filter (/= best) population
    newPopulation <- liftIO $ evolve input rem
    -- liftIO $ print newPopulation
    tell [best ++ " | (Fitness: " ++ show bestFitness ++ ")"]
    
    if bestFitness == 0 
        then return best
        else algorithm input (best:newPopulation)

-- Evolve a population with crossovers and mutations
evolve :: String -> [String] -> IO [String]
evolve input population = do
    let best = fst (fittest input population)
    result <- mapM (crossover best) population
    mapM mutate $ result

-- Return the fittest of the strings compared to the input
fittest :: String -> [String] -> (String, Int)
fittest input population = minimumBy (compare `on` snd) $ distances
    where
        distances = map (\s -> (s, hammingDistance input s)) population

-- Creates a uniform crossover between the 2 strings
crossover :: String -> String -> IO String
crossover s t = do
    g <- newStdGen
    let rs = take (length s) $ randomRs (1, 100) g
    return $ uniformCrossover s t rs

    where
        uniformCrossover :: String -> String -> [Int] -> String
        uniformCrossover [] _ _ = []
        uniformCrossover (s:ss) (t:ts) (p:ps)
            | p <= 50 = s : uniformCrossover ss ts ps
            | otherwise = t : uniformCrossover ss ts ps

-- Mutate a string's every character with a probability of 1.5%
mutate :: String -> IO String
mutate str = do
    mutations <- randomAsciiString $ length str
    probs <- sequence $ replicate (length str) (randomRIO (0, 1) :: IO Float)
    let sr = zip3 probs str mutations
    return $ map (\(p, c1, c2) -> if p <= 0.015 then c2 else c1) sr

-- Create a population of "n" number of string with "len" length 
population :: Int -> Int -> IO [String]
population n len = sequence $ replicate n $ randomAsciiString len

-- Genereates a random string with ASCII characters
randomAsciiString :: Int -> IO String
randomAsciiString len = do
    stdGen <- newStdGen
    let charCodes = take len (randomRs (32, 126) stdGen)
    return $ map chr charCodes

-- Calculates the Hamming distance of 2 strings
hammingDistance :: String -> String -> Int
hammingDistance str1 str2 = foldl foldingFunc 0 $ zip str1 str2
    where
        foldingFunc :: Int -> (Char, Char) -> Int
        foldingFunc acc chs = acc + distance (fst chs) (snd chs)

        -- Calculates the distance of 2 characters
        distance :: Char -> Char -> Int
        distance a b = if a == b then 0 else 1