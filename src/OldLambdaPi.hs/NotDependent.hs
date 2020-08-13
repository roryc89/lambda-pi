{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


module OldLambdaPi.NotDependent where 

-- -- import Data.Text
-- import Control.Monad (unless)

-- -- | Term with type produced by checking algorithm (Inferrable term)
-- data TermUp
--     = Ann TermDown Type -- | Type annotation
--     | Bound Int -- | Local term, De Bruijn indices assigned
--     | Free Name -- | Global with text name 
--     | Apply TermUp TermDown 
--     deriving (Show, Eq)

-- (@@) :: TermUp -> TermDown -> TermUp
-- a @@ b = Apply a b

-- -- | Term where type is an input to the checking algorithm (Checkable term)
-- data TermDown 
--     = Inf TermUp -- | Inferrable terms embedded in checkable terms
--     | Lambda TermDown
--     deriving (Show, Eq)

-- data Name 
--     = Global String -- | Regular globally named entity
--     | Local Int -- | Bound variable temporarily converted to free variable 
--     | Quote Int
--     deriving (Show, Eq)

-- data Type 
--     = TFree Name  -- | Type identifier
--     | Fun Type Type -- | Arrow Function
--     deriving (Show, Eq)

-- data Value 
--     = VLambda (Value -> Value) -- | Lambda abstraction
--     | VNeutral Neutral

-- -- | Neutral term, i.e. a variable applied to a (possibly empty) sequence of values
-- data Neutral
--     = NFree Name 
--     | NApply Neutral Value
    
-- vfree :: Name -> Value 
-- vfree n = VNeutral (NFree n)

-- type Env = [Value]

-- --------------------
-- -- Evaluation 
-- --------------------

-- evalUp :: TermUp -> Env -> Value 
-- evalUp t env = case t of 
--     Ann e _ -> evalDown e env
--     Free x -> vfree x
--     Bound i -> env !! i
--     Apply e1 e2 -> applyValue (evalUp e1 env) (evalDown e2 env)

-- -- originally vapp 
-- applyValue :: Value -> Value -> Value 
-- applyValue v1 v2 = case v1 of 
--     VLambda f -> f v2
--     VNeutral n -> VNeutral (NApply n v2)

-- evalDown :: TermDown -> Env -> Value 
-- evalDown t env = case t of 
--     Inf i -> evalUp i env 
--     Lambda e -> VLambda (\x -> evalDown e (x : env))

-- --------------------
-- -- Contexts 
-- --------------------

-- data Kind = Star 
--     deriving (Show)

-- data Info 
--     = HasKind Kind
--     | HasType Type 
--     deriving (Show)

-- type Context = [(Name, Info)]

-- --------------------
-- -- Type Checking 
-- --------------------

-- type Result a = Either String a 

-- throwError = Left

-- kindDown :: Context -> Type -> Kind -> Result ()
-- kindDown ctx t Star = case t of 

--     TFree x -> case lookup x ctx of 
--         Just (HasKind Star) -> return ()
--         Nothing -> throwError "unknown identifier"

--     Fun k1 k2 -> do 
--         kindDown ctx k1 Star 
--         kindDown ctx k2 Star 

-- typeUp0 :: Context -> TermUp -> Result Type
-- typeUp0 = typeUp 0 

-- -- | Infer a term's type
-- typeUp :: Int -> Context -> TermUp -> Result Type
-- typeUp i ctx termU =  

--     case termU of 
--         Ann termD t -> do 
--             kindDown ctx t Star
--             return t

--         Free x -> case lookup x ctx of 
--             Just (HasType t) -> return t
--             Nothing -> throwError "unknown identifier"

--         Apply e1 e2 -> do 
--             fnType <- typeUp i ctx e1
--             case fnType of 
--                 Fun t1 t2 -> do 
--                     typeDown i ctx e2 t1
--                     return t2
--                 _ -> throwError "Cannot apply, not a function"

-- -- | Check a term's type 
-- typeDown :: Int -> Context -> TermDown -> Type -> Result ()
-- typeDown i ctx termD suppliedType = case (termD, suppliedType) of 

--     (Inf termU, _) -> do 
--         inferredType <- typeUp i ctx termU 
--         unless (inferredType == suppliedType) (throwError "Type mismatch")

--     (Lambda lambdaTerm, Fun t1 t2) -> 
--         typeDown 
--             (i + 1) 
--             ((Local i, HasType t1) : ctx) 
--             (substDown 0 (Free (Local i)) lambdaTerm)
--             t2

--     tup -> throwError $ "type mismatch: " ++ show tup

-- substUp :: Int -> TermUp -> TermUp -> TermUp 
-- substUp i termU termUp_ = case termUp_ of 
--     Ann termD t -> Ann (substDown i termU termD) t 
--     Bound j -> if i == j then termU else Bound j
--     Free name -> Free name 
--     Apply e1 e2 -> Apply (substUp i termU e1) (substDown i termU e2)

-- substDown :: Int -> TermUp -> TermDown -> TermDown
-- substDown i termU termDown_ = case termDown_ of 
--     Inf e -> Inf (substUp i termU e)
--     Lambda e -> Lambda (substDown (i + 1) termU e)

-- --------------------
-- -- Quotation
-- --------------------

-- quote0 :: Value -> TermDown
-- quote0 = quote 0 

-- quote :: Int -> Value -> TermDown
-- quote i val = case val of 
--     VLambda f -> Lambda (quote (i + 1) (f (vfree (Quote i))))
--     VNeutral n -> Inf (neutralQuote i n)

-- neutralQuote :: Int -> Neutral -> TermUp 
-- neutralQuote i n = case n of 
--     NFree x -> boundFree i x 
--     NApply n v -> neutralQuote i n `Apply` quote i v

-- boundFree :: Int -> Name -> TermUp 
-- boundFree i n = case n of 
--     Quote k -> Bound (i - k - 1)
--     x -> Free x

-- --------------------
-- -- Lambda typed Prelude
-- --------------------

-- id_ :: TermDown
-- id_ = Lambda (Inf (Bound 0))

-- const_ :: TermDown
-- const_ = Lambda $ Lambda $ Inf $ Bound 1

-- mkFreeType :: String -> Type
-- mkFreeType a = TFree $ Global a 

-- mkTerm :: String -> TermDown
-- mkTerm x = Inf $ Free $ Global x

-- term1 :: TermUp
-- term1 = Ann id_ (Fun (mkFreeType "a") (mkFreeType "a")) `Apply` mkTerm "y"

-- term2 :: TermUp
-- term2 = Ann const_
--     (Fun (Fun (mkFreeType "b") (mkFreeType "b"))
--     (Fun (mkFreeType "a")
--     (Fun (mkFreeType "b") (mkFreeType "b"))))
--         @@ id_
--         @@ mkTerm "y"

-- env1 :: Context
-- env1 = [(Global "y", HasType (mkFreeType "a")), (Global "a", HasKind Star)]

-- env2 = (Global "b", HasKind Star) : env1