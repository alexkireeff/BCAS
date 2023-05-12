module Types where

data Expression
  = Variable String
  | Integer Int
  | Imaginary
  | Function String [Expression]
  deriving (Eq)

instance Show Expression where
  show (Variable name) = name
  show (Integer value) = show value
  show Imaginary = "i"
  show (Function name args) = name ++ " (" ++ showArgs args ++ ")"
    where
      showArgs = unwords . map show

data Equation
  = Equal Expression Expression
  deriving (Eq)

instance Show Equation where
  show (Equal lhs rhs) = show lhs ++ " = " ++ show rhs
