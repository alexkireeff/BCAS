data Expression = 
    Var String | Rat Int Int | Lim | Imag |
    Add Expression Expression | Mul Expression Expression | Sub Expression Expression | Div Expression Expression
    deriving Show

data Equation = Equals Expression Expression

