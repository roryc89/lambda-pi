{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    = Ann Term TypeTerm -- | Type annotation
    | App Term Term -- | Apply
    | Lam Text (Maybe Term) Term  -- | Lambda
    | TypeConst Text -- | Type constant such as "Int" or "List"
    | VarIdx Int -- | A bound variable using de bruijn indices
    | TypeVarDecl Text (Maybe Term) Term -- | Declare a type variable
    | Arrow Term Term -- | Arrow type (Type of a lambda)
    | Type -- | The type of types
    
    -- Literals
    | Int Int
    | String Text
    deriving (Show, Eq, Ord)


type TypeTerm = Term

data Ctr 
    = Ctr Text TypeTerm
    deriving (Show, Eq, Ord)


typeInt = TypeConst "Int"

typeString = TypeConst "String"
