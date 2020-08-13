{-# LANGUAGE OverloadedStrings #-}

module Test.LambdaPi.Infer where 

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import LambdaPi.Term
import LambdaPi.Infer

tests :: TestTree
tests = testGroup "LambdaPi.Infer"
    [ testGroup "Passing"
        [ testGroup "Basic inference" $ fmap testPassing 
            [ ("integer", Int 0, typeInt)
            
            , ("string", String "", typeString)
            
            , ("lambda - id", idLambda, VarIdx 0 `Arrow` VarIdx 0)
            
            , ( "lambda - const"
              , constLambda
              , VarIdx 0 `Arrow` (VarIdx 1 `Arrow` VarIdx 0)
              )
            
            , ("apply - id", idLambda `App` Int 1, typeInt)
            
            , ( "apply - const"
              , constLambda `App` String "hello"
              , VarIdx 0 `Arrow` typeString
              )
            ]
            
         
        ]

    , testGroup "Failing" 
        [ testGroup "Basic inference" 
            [ testCase "Int annotated" $ 
                runInferTerm (Int 1 `Ann` typeString) @?=
                    Left (TypeMismatch (TypeConst "String") (TypeConst "Int"))
            ]
        ]
    ]
    where 
        idLambda = "a" `lamU` Var "a"

        constLambda = "a" `lamU` ("b" `lamU` Var "a")

        testPassing (name, input, expected) = testCase name $ 
            runInferTerm input @?= Right expected