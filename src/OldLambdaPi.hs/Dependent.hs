{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module OldLambdaPi.Depenendent where 

import Control.Monad (unless)

-- | Term with type produced by checking algorithm (Inferrable term)
data TermUp
    = Ann TermDown TermDown -- | Type annotation
    | Star -- | Kind (Type of Types)
    | Pi TermDown TermDown
    | Bound Int -- | Local term, De Bruijn indices assigned
    | Free Name -- | Global with text name 
    | Apply TermUp TermDown 
    -- In Built terms
    | Nat -- | Natural numbers
    | NatElim TermDown TermDown TermDown TermDown -- | Natural elminator
    | Zero -- | Natural zero 
    | Succ TermDown -- | Natural successor 
    deriving (Show, Eq)

(-->) :: Int -> Int -> Int
a --> b = a + b


(@@) :: TermUp -> TermDown -> TermUp
a @@ b = Apply a b

-- | Term where type is an input to the checking algorithm (Checkable term)
data TermDown 
    = Inf TermUp -- | Inferrable terms embedded in checkable terms
    | Lambda TermDown
    deriving (Show, Eq)

data Name 
    = Global String -- | Regular globally named entity
    | Local Int -- | Bound variable temporarily converted to free variable 
    | Quote Int
    deriving (Show, Eq)

data Value 
    = VLambda (Value -> Value) -- | Lambda abstraction
    | VStar
    | VPi Value (Value -> Value)
    | VNeutral Neutral
    | VNat
    | VZero
    | VSucc Value

-- | Neutral term, i.e. a variable applied to a (possibly empty) sequence of values
data Neutral
    = NFree Name 
    | NApply Neutral Value
    | NNatElim Value Value Value Neutral
    
vfree :: Name -> Value 
vfree n = VNeutral (NFree n)

type Env = [Value]

-- --------------------
-- Evaluation 
-- --------------------

evalUp :: TermUp -> Env -> Value 
evalUp t env = case t of 
    Ann e _ -> evalDown e env
    Free x -> vfree x
    Bound i -> env !! i
    Apply e1 e2 -> applyValue (evalUp e1 env) (evalDown e2 env)
    Pi t1 t2 -> VPi (evalDown t1 env) (\x -> evalDown t2 (x : env))
    Star -> VStar
    -- Built ins
    Nat -> VNat
    Zero -> VZero
    Succ k -> VSucc (evalDown k env)
    NatElim m mz ms k ->
        let 
            mzVal = evalDown mz env
            msVal = evalDown ms env
            rec kVal = 
                case kVal of 
                    VZero -> mzVal 
                    VSucc l -> msVal `applyValue` l `applyValue` rec l 
                    VNeutral k -> VNeutral (NNatElim (evalDown m env) mzVal msVal k)
                    _ -> error "internal: eval natElim"
        in 
        rec (evalDown k env)

-- originally vapp 
applyValue :: Value -> Value -> Value 
applyValue v1 v2 = case v1 of 
    VLambda f -> f v2
    VNeutral n -> VNeutral (NApply n v2)

evalDown :: TermDown -> Env -> Value 
evalDown t env = case t of 
    Inf i -> evalUp i env 
    Lambda e -> VLambda (\x -> evalDown e (x : env))

-- --------------------
-- Contexts 
-- --------------------

type Type = Value

type Context = [(Name, Type)]

-- --------------------
-- Type Checking 
-- --------------------

type Result a = Either String a 

throwError = Left

typeUp0 :: Context -> TermUp -> Result Type
typeUp0 = typeUp 0 

-- | Infer a term's type
typeUp :: Int -> Context -> TermUp -> Result Type
typeUp i ctx termU =  

    case termU of 
        Ann termD rho -> do 
            typeDown i ctx rho VStar
            let evaledType = evalDown rho []
            typeDown i ctx termD evaledType
            return evaledType

        Pi t1 t2 -> do 
            let evaledType = evalDown t1 []
            typeDown 
                (i + 1) 
                ((Local i, evaledType) : ctx)
                (substDown 0 (Free (Local i)) t2) VStar
            return VStar

        Free x -> case lookup x ctx of 
            Just t -> return t
            Nothing -> throwError "unknown identifier"

        Apply e1 e2 -> do 
            fnType <- typeUp i ctx e1
            case fnType of 
                VPi t1 t2 -> do 
                    typeDown i ctx e2 t1
                    return (t2 (evalDown e2 []))
                _ -> throwError "Cannot apply, not a function"

        Star -> return VStar 

        Bound i -> undefined 

        Nat -> return VNat

        Zero -> return VZero

        Succ k -> do 
            typeDown i ctx k VNat
            return VNat 

        NatElim m mz ms k -> do 
            typeDown i ctx m (VPi VNat (const VStar))
            let mVal = evalDown m []
            typeDown i ctx mz (mVal `applyValue` VZero)
            typeDown i ctx ms (VPi VNat (\l -> VPi (mVal `applyValue` l) (\_ -> mVal `applyValue` VSucc l)))
            typeDown i ctx k VNat 
            let kVal = evalDown k []
            return $ mVal `applyValue` kVal
        

-- | Check a term's type 
typeDown :: Int -> Context -> TermDown -> Type -> Result ()
typeDown i ctx termD suppliedType = 
    case (termD, suppliedType) of 

        (Inf termU, _) -> do 
            inferredType <- typeUp i ctx termU 
            unless (quote0 inferredType == quote0 suppliedType) 
                (throwError "Type mismatch")

        (Lambda lambdaTerm, VPi t1 t2) -> 
            typeDown 
                (i + 1) 
                ((Local i, t1) : ctx) 
                (substDown 0 (Free (Local i)) lambdaTerm)
                (t2 (vfree (Local i)))

        (otherTerm, otherType) -> throwError 
            $ "type mismatch: " ++ show (otherTerm, quote0 otherType)

substUp :: Int -> TermUp -> TermUp -> TermUp 
substUp i termU termUp_ = case termUp_ of 
    Ann termD t -> Ann (substDown i termU termD) t 
    Bound j -> if i == j then termU else Bound j
    Free name -> Free name 
    Apply e1 e2 -> Apply (substUp i termU e1) (substDown i termU e2)
    Pi t1 t2 -> Pi (substDown i termU t1) (substDown i termU t2)
    Star -> Star
    Nat -> Nat
    Zero -> Zero
    Succ k -> Succ k
    NatElim m mz ms n ->
        NatElim 
            (substDown i termU m)
            (substDown i termU mz) 
            (substDown i termU ms)
            (substDown i termU ms)

substDown :: Int -> TermUp -> TermDown -> TermDown
substDown i termU termDown_ = case termDown_ of 
    Inf e -> Inf (substUp i termU e)
    Lambda e -> Lambda (substDown (i + 1) termU e)

-- --------------------
-- Quotation
-- --------------------

quote0 :: Value -> TermDown
quote0 = quote 0 

quote :: Int -> Value -> TermDown
quote i val = case val of 
    VLambda f -> Lambda (quote (i + 1) (f (vfree (Quote i))))
    VNeutral n -> Inf (neutralQuote i n)
    VPi v f -> Inf (Pi (quote i v) (quote (i + 1) (f (vfree (Quote i)))))
    VStar -> Inf Star
    VNat -> Inf Nat
    VZero -> Inf Zero
    VSucc k -> Inf (Succ $ quote i k)

neutralQuote :: Int -> Neutral -> TermUp 
neutralQuote i n = case n of 
    NFree x -> boundFree i x 
    NApply n v -> neutralQuote i n `Apply` quote i v
    NNatElim m z s n -> NatElim (quote i m) (quote i z) (quote i s) (Inf (neutralQuote i n))

boundFree :: Int -> Name -> TermUp 
boundFree i n = case n of 
    Quote k -> Bound (i - k - 1)
    x -> Free x

-- --------------------
-- Prelude
-- --------------------

id_ :: TermDown
id_ = Lambda (Inf (Bound 0))

const_ :: TermDown
const_ = Lambda $ Lambda $ Inf $ Bound 1

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