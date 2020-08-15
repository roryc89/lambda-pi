module Andromeda.Error where 

import Andromeda.Expr 

data TypeError 
    = TypeMismatch [Expr]
    | FunctionExpected Expr
    | TypeExpected Expr
    | UnknownVariable Variable
    -- | PatternDoesNotFitType Expr Pattern
    | Errs TypeError TypeError
    deriving (Show, Eq, Ord)