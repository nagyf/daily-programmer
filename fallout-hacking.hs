import System.Random
import Control.Monad.State
import Data.List (sortBy)

-- https://www.reddit.com/r/dailyprogrammer/comments/3qjnil/20151028_challenge_238_intermediate_fallout/?utm_content=title&utm_medium=browse&utm_source=reddit&utm_name=dailyprogrammer

-- Game difficulty
data Difficulty = VeryEasy | Easy | Average | Hard | VeryHard deriving(Show, Eq, Ord)

-- Stores the state of the game
data GameState = GameState {
    wordList :: [String],
    winnerWord :: String,
    guessesLeft :: Int,
    len :: Int
} deriving(Show)

main :: IO ()
main = do
    diff <- chooseDifficulty
    let wl = wordLength diff
    wordList <- readWords wl
    gameWords <- randomWords 6 wordList
    shuffledWords <- shuffle gameWords
    
    let gameState = GameState {
        winnerWord = head gameWords,
        wordList = shuffledWords,
        len = wl,
        guessesLeft = 4
    }

    (result, state) <- runStateT game gameState
    case result of
        True -> putStrLn "You won!"
        False -> putStrLn "Loser!"

-- The game loop
game :: StateT GameState IO Bool
game = do
    state <- get
    mapM_ (liftIO . putStrLn) (wordList state) -- Print the possible words
    liftIO $ putStr $ "Guess? (" ++ show (guessesLeft state) ++ " left): "
    guess <- liftIO getLine
    let correct = matches (winnerWord state) guess

    if correct == (len state) then return True
        else
            if gameOver state then return False
                else do
                    liftIO $ putStrLn (show correct ++ "/" ++ show (len state) ++ " correct")
                    put $ state {guessesLeft = pred $ guessesLeft state}
                    game

-- Check if the game is over
gameOver :: GameState -> Bool
gameOver state = (guessesLeft state) == 1

-- Return N number of random words from the list
randomWords :: Int -> [String] -> IO [String]
randomWords n ws = do
    g <- newStdGen
    let idxs = take n $ randomRs (0, length ws) g
    return $ map (\i -> ws !! i) idxs

-- Shuffle a list of words
shuffle :: [String] -> IO [String]
shuffle ws = do
    g <- newStdGen
    let ns = take (length ws) $ randoms g :: [Int]
    return $ map (snd) $ sortBy (\x y-> compare (fst x) (fst y)) $ zip (ns) (ws)

-- Get the difficulty from the user
chooseDifficulty :: IO Difficulty
chooseDifficulty = do
    putStr "Difficulty? (very easy, easy, average, hard, very hard): "
    answer <- readDifficulty <$> getLine
    case answer of
        (Just diff) -> return diff
        Nothing -> chooseDifficulty

readDifficulty :: String -> Maybe Difficulty
readDifficulty "very easy" = Just VeryEasy
readDifficulty "easy" = Just Easy
readDifficulty "average" = Just Average
readDifficulty "hard" = Just Hard
readDifficulty "very hard" = Just VeryHard
readDifficulty _ = Nothing

-- Returns the word length based on the difficulty
wordLength :: Difficulty -> Int
wordLength diff = case diff of
    VeryEasy -> 4
    Easy -> 7
    Average -> 10
    Hard -> 13
    VeryHard -> 15

-- Read the words from the file, only return those that's length matches the parameter number
readWords :: Int -> IO [String]
readWords len = do
    ws <- words <$> readFile "enable1.txt"
    return $ [w | w <- ws, len == length w]

distance :: String -> String -> Int
distance s [] = length s
distance [] s = length s
distance (s:ss) (t:tt)
    | s == t = distance ss tt
    | otherwise = 1 + distance ss tt

matches :: String -> String -> Int
matches s t = (length s) - (distance s t)