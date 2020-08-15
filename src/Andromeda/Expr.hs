module Andromeda.Expr where

import Data.Text (Text)

data Variable 
    = StringVar Text
    | GeneratedVar Text Int 
    | Dummy
    deriving (Show, Eq, Ord)

data Expr 
    = Var Variable
    | Universe Int
    | Pi Abstraction
    | Lam Abstraction
    | App Expr Expr
    deriving (Show, Eq, Ord)
    
type TypeExpr = Expr 

type Abstraction = (Variable, Expr, Expr)