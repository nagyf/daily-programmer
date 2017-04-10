data Term = Add | Sub | Mul | Div | IntDiv | Mod | Pow | Fact | Num Float deriving(Show)

main :: IO ()
main = do
    let input = "100 807 3 331 * + 2 2 1 + 2 + * 5 ^ * 23 10 558 * 10 * + + *"
    let terms = parse input
    print $ solveRPN terms

parse :: String -> [Term]
parse "" = []
parse s = map parseTerm $ words s
    where
        parseTerm "+" = Add
        parseTerm "-" = Sub
        parseTerm "*" = Mul
        parseTerm "x" = Mul
        parseTerm "/" = Div
        parseTerm "//" = IntDiv
        parseTerm "%" = Mod
        parseTerm "^" = Pow
        parseTerm "!" = Fact
        parseTerm x = Num (read x :: Float)

solveRPN :: [Term] -> Term
solveRPN = head . foldl solver []
    where
        solver xs (Num x) = (Num x) : xs
        solver ((Num x):xs) Fact = Num (fromInteger (factorial (round x))) : xs
        solver ((Num x):(Num y):xs) Add = Num (y + x) : xs
        solver ((Num x):(Num y):xs) Sub = Num (y - x) : xs
        solver ((Num x):(Num y):xs) Mul = Num (y * x) : xs
        solver ((Num x):(Num y):xs) IntDiv = Num (intDiv y x) : xs
        solver ((Num x):(Num y):xs) Div = Num (y / x) : xs
        solver ((Num x):(Num y):xs) Mod = Num (myMod y x) : xs
        solver ((Num x):(Num y):xs) Pow = Num (y ** x) : xs

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

myMod :: Float -> Float -> Float
myMod x y = fromInteger ((round x) `mod` (round y))

intDiv :: Float -> Float -> Float
intDiv x y = fromInteger $ round x `div` round y