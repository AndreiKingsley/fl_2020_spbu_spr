module Test.MyTests where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser)
import           LLang
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_isPrime :: Assertion
unit_isPrime = do
    runParser parseL "{read(a); ans = 1; if(a == 0 || a == 1) ans = 0 else {b = 2; while(a > b) if (a % b == 0) ans = 0; print(ans);};}" 
	@?= Success "" (Seq[Read "a", Assign "ans" (Num 1), If (BinOp Or (BinOp Equal (Ident "a") (Num 0)) (BinOp Equal (Ident "a") (Num 1))) (Assign "ans" (Num 0)) (Seq[Assign "b" (Num 2), While (BinOp Gt (Ident "a") (Ident "b")) (If (BinOp Equal (BinOp Mod (Ident "a") (Ident "b")) (Num 0)) (Assign "ans" (Num 0)) (Seq [])), Write (Ident "ans")])])

unit_digitSum :: Assertion
unit_digitSum = do
     runParser parseL "{read(a); ans = 0; while(a > 0) {ans = ans + (a % 10); a = a / 10;}; print(ans);}" 
	@?= Success "" (Seq[Read "a", Assign "ans" (Num 0), While (BinOp Gt (Ident "a") (Num 0)) (Seq[Assign "ans" (BinOp Plus (Ident "ans") (BinOp Mod (Ident "a") (Num 10))), Assign "a" (BinOp Div (Ident "a") (Num 10))]), Write (Ident "ans")])	


