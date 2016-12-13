module RunGame where
import DataTypes

import Data.Char
import Data.List.Split
import System.Random

data Interface = Interface 
    {   iNewGame    :: Int -> Int -> Int -> StdGen -> GameField 
    ,   iFlagCell   :: GameField -> Pos -> GameField
    ,   iClickCell  :: GameField -> Pos -> GameField
    ,   iHasWon     :: GameField -> Bool
    ,   iGameOver   :: GameField -> Bool
    }


-- | Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to Mine Sweeper."
  putStrLn "Enter width of the game field"
  x <- getLine 
  putStrLn "Enter height of the game field"
  y <- getLine
  putStrLn "Enter number of bombs"
  bombs <- getLine
  let xint = read x :: Int
  let yint = read y :: Int
  let nbrBombs = read bombs :: Int
  g <- newStdGen
  gameLoop i (iNewGame i xint yint nbrBombs g)

gameLoop :: Interface -> GameField -> IO ()
gameLoop i gameField = do
    printField gameField
    if (iGameOver i gameField) || (iHasWon i gameField) then do
        finish i (iHasWon i gameField)
    else do
        putStrLn "Click [C] or Flag [F] a position, [C y x] or [F y x]"
        inputLine <- getLine
        let (action, pos) = parseInput inputLine
        if (isValidInput action pos gameField) then do 
            if action == Click then do
                gameLoop i (iClickCell i gameField pos)
            else do
                gameLoop i (iFlagCell i gameField pos)
        else do
            putStrLn ("Invalid input")
            gameLoop i gameField

isValidInput :: Action -> Pos -> GameField -> Bool
isValidInput Invalid _ _                = False
isValidInput _ (y, x) (GameField rows)  = not (y < 0 || y >= yMax || x < 0 || x >= xMax)
    where 
        yMax = length rows
        xMax = length (rows !! 0)

parseInput :: String -> (Action, Pos)
parseInput s = 
    if valid then
        if head inputs == "F" then
            (Flag, pos)
        else if head inputs == "C" then
            (Click, pos)
        else
            (Invalid, pos)
    else
        (Invalid, (-1,-1))
    where 
        inputs = splitOn " " s
        valid = length inputs == 3
        pos = (read (inputs !! 1) :: Int, read (inputs !! 2) :: Int)

finish :: Interface -> Bool -> IO ()
finish i didWin = do
    if didWin 
        then 
            putStrLn ("YOU WON")
        else
            putStrLn("YOU LOST")

printField :: GameField -> IO ()
printField (GameField rows) = do
        putStrLn ( foldl (++) "    " [digs i ++ " " | (i, _) <- zip [0..] (rows !! 0)])   
        putStrLn ( foldl (++) "    " ["___" | _ <- (rows !! 0)])   
        putStrLn (unlines [ foldl (++) ((digs rowNum) ++ [' ', '|', ' ']) [[cellToChar c] ++ "  " | c <- row ] | (rowNum, row) <- zip [0..] rows] )

cellToChar :: Cell -> Char
cellToChar (Cell Closed _)              = '.'
cellToChar (Cell Flagged _)             = 'P'
cellToChar (Cell Opened (Numeric 0))    = ' ' 
cellToChar (Cell _ (Numeric n))         = intToDigit n
cellToChar _                            = '+'  

digs :: Int -> [Char]
digs 0 = ['0', '0']
digs x = 
    if x < 10 then 
        ['0'] ++ digs' x
    else
        digs' x

digs' :: Int -> [Char]
digs' 0 = []
digs' x = digs' (x `div` 10) ++ [intToDigit (x `mod` 10)]    