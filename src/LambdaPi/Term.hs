{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module LambdaPi.Term where 

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Text (Text)


data Statement 
    = Decl Text Term
    | TypeDecl Text [Ctr] -- | Creation of a new type 
    deriving (Show, Eq, Ord)


data StatementTyped = DeclTyped Text Term Term
    deriving (Show, Eq, Ord)

data Term 
    = Ann Term Term -- | Type annotation
    | App Term Term -- | Apply
    | Lam Text (Maybe Term) Term  -- | Lambda
    | Var Text -- | Used variable
    | TypeConst Text -- | Type constant such as "Int" or "List"
    | VarIdx Int -- | A bound variable using de bruijn indices
    | TypeVarDecl Text (Maybe Term) Term -- | Declare a type variable
    | Arrow Term Term -- | Arrow type (Type of a lambda)
    | Type -- | The type of types
    -- Literals
    | Int Int
    | String Text
    -- Native
    | NatIntPlus Term Term
    | NatIntSub Term Term
    | NatIntMul Term Term
    | NatStringSlice Term Term Term
    | NatStringConcat Term Term
    deriving (Show, Eq, Ord)

mapTerms :: (Term -> Term) -> Term -> Term
mapTerms f t = f $ case t of 
    Ann t1 t2 -> Ann (mapTerms f t1) (mapTerms f t2)
    App t1 t2 -> App (mapTerms f t1) (mapTerms f t2)
    Lam txt tMay t2 -> Lam txt (fmap (mapTerms f) tMay) (mapTerms f t2)
    Var txt -> Var txt
    TypeConst txt -> TypeConst txt
    VarIdx int -> VarIdx int
    TypeVarDecl txt tMay t2 -> TypeVarDecl txt (fmap (mapTerms f) tMay) (mapTerms f t2) 
    Arrow t1 t2 -> Arrow (mapTerms f t1) (mapTerms f t2)
    Type  -> Type 
    Int int -> Int int
    String txt -> String txt
    NatIntPlus t1 t2 -> NatIntPlus (mapTerms f t1) (mapTerms f t2)
    NatIntSub t1 t2 -> NatIntSub (mapTerms f t1) (mapTerms f t2)
    NatIntMul t1 t2 -> NatIntMul (mapTerms f t1) (mapTerms f t2)
    NatStringSlice t1 t2 t3 -> NatStringSlice (mapTerms f t1) (mapTerms f t2) (mapTerms f t3)
    NatStringConcat t1 t2 -> NatStringConcat (mapTerms f t1) (mapTerms f t2)

replaceIdxWith :: Int -> Term -> Term -> Term
replaceIdxWith id replacement = mapTerms (\t -> case t of 
    VarIdx i | i == id -> replacement
    _ -> t)

replaceVarIdxWith :: Term -> Term -> Term -> Term
replaceVarIdxWith (VarIdx id) = replaceIdxWith id
replaceVarIdxWith _ = \_ t -> t

    
-- | Untyped lambda (without annotation)
lamU :: Text -> Term -> Term
lamU arg = Lam arg Nothing

type TypeTerm = Term

data Ctr 
    = Ctr Text TypeTerm
    deriving (Show, Eq, Ord)


typeInt = TypeConst "Int"

typeString = TypeConst "String"
