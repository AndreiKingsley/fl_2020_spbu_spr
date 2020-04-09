module Test.MyTests where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser)
import           LLang
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False


unit_mod :: Assertion
unit_mod = do 
    runParser parseProg "Seq {Read (a); Read (b); While (a > b) (Seq{Assign (a) (a-b);}); Write (a);}"  
	@?= Success "" (Seq [Read "a", Read "b", While (BinOp Gt (Ident "a") (Ident "b")) (Seq[Assign "a" (BinOp Minus (Ident "a") (Ident "b"))]), Write (Ident "a")])

unit_swap :: Assertion
unit_swap = do
    runParser parseProg "Seq {Read (a); Read (b); Assign (x) (b); Assign (b) (a); Assign (a) (x); Write(a); Write(b);}"
	@?= Success "" (Seq [Read "a", Read "b", Assign "x" (Ident "b"), Assign "b" (Ident "a"), Assign "a" (Ident "x"), Write (Ident "a"), Write (Ident "b")])

unit_gcd :: Assertion
unit_gcd = do 
    runParser parseProg "Seq {Read (a); Read (b); While (b) (Seq {While (a > b) (Seq{Assign (a) (a-b);}); Assign (x) (b); Assign (b) (a); Assign (a) (x);}); Write (a);}" 
	@?= Success "" (Seq [Read "a", Read "b", While (Ident "b") (Seq [While (BinOp Gt (Ident "a") (Ident "b")) (Seq[Assign "a" (BinOp Minus (Ident "a") (Ident "b"))]), Assign "x" (Ident "b"), Assign "b" (Ident "a"), Assign "a" (Ident "x")]), Write (Ident "a")])


