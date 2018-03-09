module Expression
  where

data Expression
  = Number Double
  | Sum Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Minus Expression
  | Exp Wyrazenie