module Types where

data Expression =
    Variable String | Integer Int | Imaginary | Function String [Expression]
    deriving (Show, Eq)

data Equation =
    Equal Expression Expression
    deriving (Show, Eq)
