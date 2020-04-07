module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol, stringParser)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent, OpType (..))
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import		 LLang		     

isFailure (Failure _) = True
isFailure  _          = False

unit_parseLNum :: Assertion
unit_parseLNum = do
    runParser parseLNum "$SEREBRYACHOK$" @?= Success "" 7
    runParser parseLNum "$CELKOVIY$$NOL$" @?= Success "" 10
    runParser parseLNum "$POLUSHKA$+$POLUSHKA$" @?= Success "+$POLUSHKA$" 2
    assertBool "" $ isFailure (runParser parseLNum "4")
    assertBool "" $ isFailure (runParser parseLNum "DEVYATICHOK")

unit_parseLIdent :: Assertion
unit_parseLIdent = do
    runParser parseLIdent "@uRSU@" @?= Success "" "uRSU"
    runParser parseLIdent "@ussuu@" @?= Success "" "ussuu"
    runParser parseLIdent "@@tox" @?= Success "tox" ""
    assertBool "" $ isFailure (runParser parseLNum "@r")
    assertBool "" $ isFailure (runParser parseLNum "urs")
    assertBool "" $ isFailure (runParser parseLNum "@SurS@")
    assertBool "" $ isFailure (runParser parseLNum "@aSS@")


unit_parseLExpr :: Assertion
unit_parseLExpr = do
    runParser parseLExpr ":$CELKOVIY$*$POLUSHKA$*$CHETVERTUSHKA$:"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    runParser parseLExpr ":$CELKOVIY$$POLUSHKA$$CHETVERTUSHKA$:"     @?= Success "" (Num 123)
    runParser parseLExpr ":@uRR@:"     @?= Success "" (Ident "uRR")
    runParser parseLExpr ":$CELKOVIY$*$POLUSHKA$+$CHETVERTUSHKA$*$OSMUSHKA$:" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    runParser parseLExpr ":$CELKOVIY$*@R@*$CHETVERTUSHKA$:"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Ident "R")) (Num 3))
    assertBool "" $ isFailure (runParser parseLExpr "$CELKOVIY$+$CELKOVIY$")


unit_parseL :: Assertion
unit_parseL = do
   runParser parseL "{#ROBIT#{#PUSTO#}}" @?= Success "" Seq {statements = [Empty]}
   runParser parseL "{#ROBIT#{#ZVYAZATI#@ruSS@:$CELKOVIY$*$POLUSHKA$*$CHETVERTUSHKA$:}{#PAKUL#:$NOL$:{#ROBIT#{#NAPISATNABERESTU#:-@ruSS@:}}}}"
	@?= Success "" Seq {statements = [Assign {var = "ruSS", expr = (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))}, While {cond = (Num 0), body = Seq{statements = [Write {expr = (UnaryOp Minus (Ident "ruSS"))}]}}]}