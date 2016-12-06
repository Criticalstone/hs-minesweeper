module RunGame where
import DataTypes

data Interface = Interface 
    {   iNewGame    :: GameField 
    ,   iFlagCell   :: GameField -> Pos -> GameField
    ,   iClickCell  :: GameField -> Pos -> GameField
    ,   iHasWon     :: GameField -> Bool
    ,   iGameOver   :: GameField -> Bool
    }