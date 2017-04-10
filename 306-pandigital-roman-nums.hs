-- https://www.reddit.com/r/dailyprogrammer/comments/5z4f3z/20170313_challenge_306_easy_pandigital_roman/?utm_content=title&utm_medium=hot&utm_source=reddit&utm_name=dailyprogrammer

main :: IO ()
main = print [x | x <- [1..2000], isPandigital x]

isPandigital :: Int -> Bool
isPandigital x = let 
        num = toRoman x 
        set = ['M', 'C', 'D', 'X', 'L', 'I', 'V']
        res = map (count num) set
    in (length res) == (length set) && all (\x -> x == 1) res

count :: (Eq a) => [a] -> a -> Int
count [] _ = 0
count (x:xs) y
    | x == y = 1 + count xs y
    | otherwise = count xs y 

romanNumerals :: [(Int, String)]
romanNumerals = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"),
                 (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

toRoman :: Int -> String
toRoman x = toRomanHelper x romanNumerals
    where
        toRomanHelper :: Int -> [(Int, String)] -> String
        toRomanHelper x [] = ""
        toRomanHelper 0 _ = ""
        toRomanHelper x ((i, s):xs) = 
            (concat $ replicate (x `div` i) s) ++ toRomanHelper (x `mod` i) xs 
                