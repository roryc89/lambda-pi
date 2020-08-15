module Andromeda.Expr where

import Data.Text (Text)

data Variable 
    = StringVar Text -- | User defined variable
    | GeneratedVar Text Int -- | Generated variable 
    | Dummy -- | Unused varible
    deriving (Show, Eq, Ord)

data Expr 
    = Var Int -- | de Briujn index
    | Subst Substitution Expr
    | Universe Int
    | Pi Abstraction
    | Lam Abstraction
    | App Expr Expr
    -- -- Literals 
    -- | Int Int 
    -- -- | String Text
    -- | IntT
    -- -- | StringT
    deriving (Show, Eq, Ord)
    
type TypeExpr = Expr 

-- An abstraction `(x,t,e)` indicates that `x` of type `t` is bound in `e`. We also keep around
--     the original name `x` of the bound variable for pretty-printing purposes.
type Abstraction = (Variable, TypeExpr, Expr)

data Substitution
    = Shift Int -- shifts the indices in [e] by [k] places
    | Dot Expr Substitution
    deriving (Show, Eq, Ord)

-- Identity substituion
idSubst = Shift 0

composeSub :: Substitution -> Substitution -> Substitution
composeSub s t = 
    case (s, t) of 
        (s, Shift 0) -> s 
        (Dot e s, Shift m) -> composeSub s (Shift (m - 1))
        (Shift m, Shift n) -> Shift (m + n)
        (s, Dot e t) -> Dot (Subst s e) (composeSub s t)

-- | `subst` applies explicit substitution `s`in expression `e`. It does so
-- |   lazily, i.e., it does just enough to expose the outermost constructor of `e`.
subst :: Substitution -> Expr -> Expr
subst s e = 
    case (s, e) of 
       (Shift m, Var k) -> Var (k + m)
       (Dot a s, Var 0) -> subst idSubst a 
       (s, Subst t e) -> subst s (subst t e)
       (_, Universe _) -> e
       (s, Pi a) -> Pi (substAbstraction s a)
       (s, Lam a) -> Lam (substAbstraction s a)
       (s, App e1 e2) -> App (Subst s e1) (Subst s e2)

    where 
        substAbstraction s (x, e1, e2) = 
            let 
                e1 = Subst s e1 
                e2 = Subst (Dot (Var 0) (composeSub (Shift 1) s)) e2 
            in
            (x, e1, e2)

-- | occurs returns `True` when variable `Var k` occurs freely in `e`.
occurs :: Int -> Expr -> Bool
occurs k e = 
    case e of 
        Var m -> m == k 
        Subst s e -> occurs k (subst s e)
        Universe _ -> False 
        Pi a -> occursAbstraction k a 
        Lam a -> occursAbstraction k a 
        App e1 e2 -> occurs k e1 || occurs k e2

occursAbstraction :: Int -> Abstraction -> Bool
occursAbstraction k (_, e1, e2) = 
    occurs k e1 || occurs k e2