import Control.Monad.Writer

main :: IO ()
main = putStrLn $ unlines $ execWriter $ game 31337357

-- https://www.reddit.com/r/dailyprogrammer/comments/3r7wxz/20151102_challenge_239_easy_a_game_of_threes/?utm_content=title&utm_medium=browse&utm_source=reddit&utm_name=dailyprogrammer
game :: Int -> Writer [String] Int
game n 
    | n == 1 = tell ["1"] >> return 1
    | n `mod` 3 == 0 = let nn = n `div` 3 in 
        tell [show n ++ " 0"] >> game nn

    | (n+1) `mod` 3 == 0 = let nn = (n+1) `div` 3 in 
        tell [show n ++ " +1"] >> game nn

    | otherwise = let nn = (n-1) `div` 3 in 
        tell [show n ++ " -1"] >> game nn
