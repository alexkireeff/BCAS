data Expression = 
    Variable String | Integer Int | Limit String | Imaginary | Function String [Expression]
    deriving Show

data Equation = 
    Equal Expression Expression
    deriving Show


