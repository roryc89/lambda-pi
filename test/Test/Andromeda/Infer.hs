{-# LANGUAGE OverloadedStrings #-}

module Test.Andromeda.Infer where 

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Andromeda.Expr
import Andromeda.Infer

-- tests :: TestTree
-- tests = testGroup "Andromeda.Infer"
--     [ testGroup "inferExpr passing"
--         [ runInferType ()
--         ]
--     ]
--     where 
--         idLambda = "a" `lamU` Var "a"

--         constLambda = "a" `lamU` ("b" `lamU` Var "a")

--         constLambdaInt = Lam "a" (Just typeInt) ("b" `lamU` Var "a")

--         testPassing (name, input, expected) = testCase name $ 
--             runInferExpr input @?= Right expected