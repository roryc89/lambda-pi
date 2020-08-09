module LambdaPi.Main where 

-- import Data.Text

-- | Term with type produced by checking algorithm (Inferrable term)
data TermUp
    = Ann TermDown Type -- | Type annotation
    | Bound Int -- | Local term, De Bruijn indices assigned
    | Free Name -- | Global with text name 
    | Apply TermUp TermDown 
    deriving (Show, Eq)

-- | Term where type is an input to the checking algorithm (Checkable term)
data TermDown 
    = Inf TermUp -- | Inferrable terms embedded in checkable terms
    | Lamda TermDown
    deriving (Show, Eq)

data Name 
    = Global String -- | Regular globally named entity
    | Local Int -- | Bound variable temporarily converted to free variable 
    | Quote Int
    deriving (Show, Eq)

data Type 
    = Tree Name  -- | Type identifier
    | Fun Type Type -- | Arrow Function
    deriving (Show, Eq)

data Value 
    = VLam (Value -> Value) -- | Lambda abstraction
    | VNeutral Neutral

-- | Neutral term 
data Neutral
    = NFree Name 
    | NApp Neutral Value
    
vfree :: Name -> Value 
vfree n = VNeutral (NFree n)

