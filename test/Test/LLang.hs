module Test.LLang where

import           AST
import           Combinators      
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           Expr
import           LLang            
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)




isFailure (Failure _) = True
isFailure  _          = False

assertParsing :: (Eq res, Show res) => Parser String String res -> String -> res -> Assertion
assertParsing parser string res = do
    runParser parser string @?= Success (toStream "" (length string)) res

unit_parseL :: Assertion
unit_parseL = do
	assertParsing parseL "{#ROBIT#{#PUSTO#}}" (Seq [Seq[]])
	assertParsing parseL "{#ROBIT#{#ZVYAZATI#@ruSS@:$CELKOVIY$*$POLUSHKA$*$CHETVERTUSHKA$:}{#PAKUL#:$NOL$:{#ROBIT#{#NAPISATNABERESTU#:-@ruSS@:}}}}" (Seq {statements = [Assign {var = "ruSS", expr = (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))}, While {cond = (Num 0), body = Seq{statements = [Write {expr = (UnaryOp Minus (Ident "ruSS"))}]}}]})
	assertParsing parseL "{#ROBIT#{#ZVYAZATI#@USSR@:$CELKOVIY$$DEVYATICHOK$$DEVYATICHOK$$CELKOVIY$+-$CELKOVIY$$DEVYATICHOK$$POLUSHKA$$POLUSHKA$:}}" (Seq {statements = [Assign {var = "USSR", expr = (BinOp Plus (Num 1991) (UnaryOp Minus (Num 1922)))}]})
	assertParsing parseL "{#ROBIT#{#KOLI#:@USSR@>$CELKOVIY$$NOL$$NOL$:#TADI#{#ZVYAZATI#@USSR@:$CELKOVIY$+$NOL$-$NOL$:}#PO-INOMU#{#PUSTO#}}}" (Seq {statements = [If {cond = BinOp Gt (Ident "USSR") (Num 100), thn = Assign {var = "USSR", expr = BinOp Minus (BinOp Plus (Num 1)(Num 0)) (Num 0)}, els = Seq {statements = []}}]})
	assertParsing parseL "{#ROBIT#{#PAKUL#:@USSR@:{#NAPISATNABERESTU#:$ZOLOTNICHOK$:}}}" (Seq {statements = [While {cond = (Ident "USSR"), body = Write {expr = (Num 8)}}]})
	assertParsing parseL "{#ROBIT#{#ZVYAZATI#@USSR@:$CELKOVIY$$DEVYATICHOK$$DEVYATICHOK$$CELKOVIY$+-$CELKOVIY$$DEVYATICHOK$$POLUSHKA$$POLUSHKA$:}{#KOLI#:@USSR@>$CELKOVIY$$NOL$$NOL$:#TADI#{#ZVYAZATI#@USSR@:$CELKOVIY$+$NOL$-$NOL$:}#PO-INOMU#{#PUSTO#}}{#PAKUL#:@USSR@:{#NAPISATNABERESTU#:$ZOLOTNICHOK$:}}}" (Seq {statements = [Assign {var = "USSR", expr = (BinOp Plus (Num 1991) (UnaryOp Minus (Num 1922)))}, If {cond = BinOp Gt (Ident "USSR") (Num 100), thn = Assign {var = "USSR", expr = BinOp Minus (BinOp Plus (Num 1)(Num 0)) (Num 0)}, els = Seq {statements = []}}, While {cond = (Ident "USSR"), body = Write {expr = (Num 8)}}]})


unit_parseDef :: Assertion
unit_parseDef = do
	assertParsing parseDef "{#VIZNACH#_ruSU_(@r@)(@u@){#ROBIT#{#PUSTO#}}{#VIDDAI#:$NOL$:}}" (Function "_ruSU_" ["r", "u"] (Seq [Seq []]) (Num 0))
	assertParsing parseDef "{#VIZNACH#_USSR_{#ROBIT#{#PAKUL#:@USSR@:{#NAPISATNABERESTU#:$ZOLOTNICHOK$:}}}{#VIDDAI#:$NOL$:}}" (Function "_USSR_" [] (Seq {statements = [While {cond = (Ident "USSR"), body = Write {expr = (Num 8)}}]}) (Num 0))

unit_parseProg :: Assertion
unit_parseProg = do
	assertParsing parseProg "~SHUE_PPSH~{#VIZNACH#_ruSU_(@r@)(@u@){#ROBIT#{#PUSTO#}}{#VIDDAI#:$NOL$:}}{#ROBIT#{#PUSTO#}}" (Program [(Function "_ruSU_" ["r", "u"] (Seq[Seq []]) (Num 0))] (Seq [Seq[]]))
	assertParsing parseProg "~SHUE_PPSH~{#VIZNACH#__{#ROBIT#{#PAKUL#:@USSR@:{#NAPISATNABERESTU#:$ZOLOTNICHOK$:}}}{#VIDDAI#:$NOL$:}}{#ROBIT#{#KOLI#:@USSR@>$CELKOVIY$$NOL$$NOL$:#TADI#{#ZVYAZATI#@USSR@:$CELKOVIY$+$NOL$-$NOL$:}#PO-INOMU#{#PUSTO#}}}" (Program [(Function "__" [] (Seq {statements = [While {cond = (Ident "USSR"), body = Write {expr = (Num 8)}}]}) (Num 0))] (Seq {statements = [If {cond = BinOp Gt (Ident "USSR") (Num 100), thn = Assign {var = "USSR", expr = BinOp Minus (BinOp Plus (Num 1)(Num 0)) (Num 0)}, els = Seq {statements = []}}]}))


unit_evalFunction :: Assertion
unit_evalFunction = do
	let f = Function "f" ["x", "y"] (Read "z") (BinOp Plus (Ident "x") (BinOp Mult (Ident "z") (Ident "y")))
	evalFunction f [2, 3] (Conf Map.empty [1] [] Map.empty) @?= Just((Conf Map.empty [] [] Map.empty), 4)
