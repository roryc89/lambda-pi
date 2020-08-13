module LambdaPi.Env where

-- (
--   Env(..),
--   empty,
--   lookup,
--   remove,
--   extend,
--   extends,
--   merge,
--   mergeEnvs,
--   singleton,
--   keys,
--   fromList,
--   toList,
-- ) where

-- import Prelude hiding (lookup)

-- -- import LambdaPi.Expr
-- -- import LambdaPi.Type

-- import Data.Monoid
-- import Data.Foldable hiding (toList)
-- import qualified Data.Map as Map
-- import Data.Text (Text)
-- import LambdaPi.Term

-- -------------------------------------------------------------------------------
-- -- Typing Environment
-- -------------------------------------------------------------------------------

-- newtype Env = Env 
--   { vars :: Map.Map Name Term 
--   }
--   deriving (Eq, Show)

-- -- data Forall = Forall [TVar] Term
-- --   deriving (Show, Eq, Ord)
  
-- empty :: Env
-- empty = Env Map.empty

-- extend :: Env -> (Name, Term) -> Env
-- extend env (x, s) = env { vars = Map.insert x s (vars env) }

-- remove :: Env -> Name -> Env
-- remove (Env env) var = Env (Map.delete var env)

-- extends :: Env -> [(Name, Term)] -> Env
-- extends env xs = env { vars = Map.union (Map.fromList xs) (vars env) }

-- lookup :: Name -> Env -> Maybe Term
-- lookup key (Env tys) = Map.lookup key tys

-- merge :: Env -> Env -> Env
-- merge (Env a) (Env b) = Env (Map.union a b)

-- mergeEnvs :: [Env] -> Env
-- mergeEnvs = foldl' merge empty

-- singleton :: Name -> Term -> Env
-- singleton x y = Env (Map.singleton x y)


-- keys :: Env -> [Name]
-- keys (Env env) = Map.keys env

-- fromList :: [(Name, Term)] -> Env
-- fromList xs = Env (Map.fromList xs)

-- toList :: Env -> [(Name, Term)]
-- toList (Env env) = Map.toList env

-- instance Semigroup Env where
--   (<>) = merge

-- instance Monoid Env where
--   mempty = empty

-- type Name = Text

-- type TVar = Text