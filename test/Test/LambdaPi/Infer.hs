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
    [ testGroup "inferTerm passing"
        [ testGroup "Basic inference" $ fmap testPassing 
            [ ("integer", Int 0, typeInt)
            
            , ("string", String "", typeString)
            
            , ("lambda - id", idLambda, VarIdx 0 `Arrow` VarIdx 0)
            
            , ( "lambda - const"
              , constLambda
              , VarIdx 0 `Arrow` (VarIdx 1 `Arrow` VarIdx 0)
              )

            , ( "lambda - const with Int annotation"
              , constLambdaInt
              , typeInt `Arrow` (VarIdx 0 `Arrow` typeInt)
              )
              
            , ( "lambda - const with Int annotation in body"
              , "a" `lamU` ("b" `lamU` (Var "a" `Ann` typeInt))
              , typeInt `Arrow` (VarIdx 1 `Arrow` typeInt)
              )

            , ("apply - id", idLambda `App` Int 1, typeInt)
            
            , ( "apply - const"
              , constLambda `App` String "hello"
              , VarIdx 1 `Arrow` typeString
              )

            , ( "apply - const twice"
              , constLambda `App` String "hello" `App` Int 1
              , typeString
              )

            , ( "apply - const with type arg"
              , constLambda `App` typeString
              , VarIdx 1 `Arrow` Type
              )

            ]
            
        ]

    , testGroup "inferTerm failing" 
        [ testGroup "Basic inference" 
            [ testCase "Int annotated as string" $ 
                runInferTerm (Int 1 `Ann` typeString) @?=
                    Left (TypeMismatch [TypeConst "Int",TypeConst "String"])

            , testCase "Applying to a string" $ 
                runInferTerm (String "str" `App` typeString) @?=
                    Left (NotAFunction (String "str"))

            , testCase "Applying twice to id" $ 
                runInferTerm ((idLambda `App` typeString) `App` typeString) @?=
                    Left (NotAFunction (App (Lam "a" Nothing (Var "a")) (TypeConst "String")))

            , testCase "Applying an incorrect type to an annotated function" $ 
                runInferTerm (constLambdaInt `App` typeString) @?=
                    Left (TypeMismatch [TypeConst "Int",TypeConst "String"])

            , testCase "A lambda that attempts to use an argument in 2 different ways, as lambda and a string" $ 
                runInferTerm ("a" `lamU` ("b" `lamU` (Var "a" `App` Var "b"))) @?=
                    Left (NotAFunction (Ann (Var "a") (TypeConst "String")))
            ]
        ]
    ]
    where 
        idLambda = "a" `lamU` Var "a"

        constLambda = "a" `lamU` ("b" `lamU` Var "a")

        constLambdaInt = Lam "a" (Just typeInt) ("b" `lamU` Var "a")

        testPassing (name, input, expected) = testCase name $ 
            runInferTerm input @?= Right expected