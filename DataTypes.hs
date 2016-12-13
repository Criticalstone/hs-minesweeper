module DataTypes where 

data State = Opened | Closed | Flagged
                deriving (Eq, Show)

data Value = Numeric Int | Bomb
                deriving (Eq, Show)

data Cell = Cell { state :: State, value :: Value }
                deriving (Eq, Show)

data GameField = GameField { rows :: [[Cell]] }
                deriving (Eq, Show)

data Action = Click | Flag | Invalid
                deriving (Eq, Show)

type Pos = (Int, Int)