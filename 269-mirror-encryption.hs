import Prelude hiding(Left, Right)
import Control.Monad.State
import qualified Data.Array.IArray as A
import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Field = Empty | Trace | MirrorLeft | MirrorRight deriving(Show, Eq)
data Direction = Up | Down | Left | Right deriving(Show)
type FieldArray = A.Array (Int, Int) Field
type Position = (Int, Int)
data MappingState = MappingState {
    mirrorField :: FieldArray,
    direction :: Direction,
    pos :: Position
}

main :: IO ()
main = do
    fileContent <- readFile "269-input.txt"
    let mf = parse fileContent
    let mapping = generateMapping mf
    word <- getLine
    putStrLn $ decode mapping word

decode :: [(Char, Char)] -> String -> String
decode chs str = map (fromJust . (flip lookup) chs) str

parse :: String -> FieldArray
parse s = let 
        list = concatMap parseLine $ lines s 
        indexed = zip (A.range ((0,0), (12,12))) list
    in 
        A.array ((0,0), (12,12)) indexed

    where
        parseLine line = map parseField line
        parseField field = case field of
            ' '  -> Empty
            '\\' -> MirrorLeft
            '/'  -> MirrorRight

generateMapping :: FieldArray -> [(Char, Char)]
generateMapping mirrorField = map generate letters 
    where
        generate ch = (ch, evalState trace (createState ch))
        createState ch = let (pos, dir) = initialPosDir ch in MappingState {pos=pos, direction=dir, mirrorField=mirrorField}
        letters = ['a'..'z'] ++ ['A'..'Z']

traceChar :: Char -> FieldArray -> IO ()
traceChar ch mf = do
    let state = createState ch mf
    let result = execState trace state
    prettyPrint $ mirrorField result

    where
        createState ch mf = let (pos, dir) = initialPosDir ch in MappingState {pos=pos, direction=dir, mirrorField=mf}

trace :: State MappingState Char
trace = do
    state <- get
    let p = (pos state)
    let field = ((mirrorField state) A.! p)
    let newDirection = changeDirection p (direction state) field
    let newPos = advancePosition p newDirection

    put $ state {mirrorField = (mirrorField state) A.// [(p, if field == Empty then Trace else field)]}

    newState <- get
    if terminalPosition newPos 
        then return $ getCharacter newPos
        else do
            put $ newState {pos = newPos, direction = newDirection}
            trace

prettyPrint :: FieldArray -> IO ()
prettyPrint arr = mapM_ printLine $ groupN 13 $ A.elems arr
    where
        printLine l = mapM_ printField l >> putStrLn ""

        printField Empty = putStr " "
        printField Trace = putStr "."
        printField MirrorLeft = putStr "\\"
        printField MirrorRight = putStr "/"


groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)

advancePosition :: Position -> Direction -> Position
advancePosition (y,x) Up = (y - 1, x)
advancePosition (y,x) Down = (y + 1, x)
advancePosition (y,x) Left = (y, x - 1)
advancePosition (y,x) Right = (y, x + 1)

changeDirection :: Position -> Direction -> Field -> Direction
changeDirection _ dir Empty = dir
changeDirection _ dir Trace = dir
changeDirection pos dir MirrorLeft = case dir of
                                        Up -> Left
                                        Down -> Right
                                        Left -> Up
                                        Right -> Down
changeDirection pos dir MirrorRight = case dir of
                                        Up -> Right
                                        Down -> Left
                                        Left -> Down
                                        Right -> Up

terminalPosition :: Position -> Bool
terminalPosition (y, x) = x == -1 || y == -1 || x == 13 || y == 13

getCharacter :: Position -> Char
getCharacter (-1, x) = ['a'..'m'] !! x
getCharacter (13, x) = ['N'..'Z'] !! x
getCharacter (y, -1) = ['A'..'M'] !! y
getCharacter (y, 13) = ['n'..'z'] !! y

initialPosDir :: Char -> (Position, Direction)
initialPosDir ch
    | ch `elem` ['a'..'m'] = ((0, fromJust $ elemIndex ch ['a'..'m']), Down)
    | ch `elem` ['N'..'Z'] = ((12, fromJust $ elemIndex ch ['N'..'Z']), Up)
    | ch `elem` ['A'..'M'] = ((fromJust $ elemIndex ch ['A'..'M'], 0), Right)
    | ch `elem` ['n'..'z'] = ((fromJust $ elemIndex ch ['n'..'z'], 12), Left)