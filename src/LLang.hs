module LLang where

import AST (AST (..), Operator (..))
import Combinators 
import Control.Applicative
import Expr

lDigits :: [String]
lDigits = ["$NOL$", "$CELKOVIY$", "$POLUSHKA$", "$CHETVERTUSHKA$", "$OSMUSHKA$", "$PUDOVICHOK$", "$MEDYACHOK$", "$SEREBRYACHOK$", "$ZOLOTNICHOK$", "$DEVYATICHOK$"]


lDigitToInt :: String -> Int
lDigitToInt x = case x of
	"$NOL$" -> 0
	"$CELKOVIY$" -> 1
	"$POLUSHKA$" -> 2
   	"$CHETVERTUSHKA$" -> 3
	"$OSMUSHKA$" -> 4
	"$PUDOVICHOK$" -> 5
	"$MEDYACHOK$" -> 6
	"$SEREBRYACHOK$" -> 7
	"$ZOLOTNICHOK$" -> 8
	"$DEVYATICHOK$" -> 9

parseLNum :: Parser String String Int
parseLNum = foldl (\acc d -> 10 * acc + lDigitToInt d) 0 <$> some (dictParser lDigits)


lAlph :: [Char]
lAlph = ['r', 'u', 's', 'R', 'U', 'S']

parserus ::Parser String String Char
parserus = satisfy $ \x -> elem x ['r', 'u', 's']

parseRUS ::Parser String String Char
parseRUS = satisfy $ \x -> elem x ['R', 'U', 'S']

parseLIdent :: Parser String String String
parseLIdent = do
	symbol '@'
        s1 <- many parserus
        s2 <- many parseRUS
	symbol '@'
	return $ s1 ++ s2


parseLExpr :: Parser String String AST
parseLExpr = do
	symbol ':'
	ast <- helper
	symbol ':'
        return ast



helper = uberExpr [
                      (orParser, Binary RightAssoc),
                      (andParser, Binary RightAssoc),
                      (notParser, Unary),
                      (equalParser <|> nequalParser <|> leParser <|> geParser <|> gtParser <|> ltParser, Binary NoAssoc),
                      (plusParser <|> minusParser, Binary LeftAssoc),
                      (multParser <|> divParser, Binary LeftAssoc),
		      (minusParser, Unary),
                      (powParser, Binary RightAssoc)
                     ]
                     (Num <$> parseLNum <|> Ident <$> parseLIdent <|> symbol '(' *> parseLExpr <* symbol ')')
                     BinOp UnaryOp
	where 
		plusParser    = stringParser "+" >>= toOperator
		multParser    = stringParser "*" >>= toOperator
		minusParser   = stringParser "-" >>= toOperator
		divParser     = stringParser "/" >>= toOperator
		powParser     = stringParser "^" >>= toOperator
		equalParser   = stringParser "==" >>= toOperator
		nequalParser  = stringParser "/=" >>= toOperator
		gtParser      = stringParser ">" >>= toOperator
		ltParser      = stringParser "<" >>= toOperator
		geParser      = stringParser ">=" >>= toOperator
		leParser      = stringParser "<=" >>= toOperator
		andParser     = stringParser "&&" >>= toOperator
		orParser      = stringParser "||" >>= toOperator
		notParser     = stringParser "!" >>= toOperator


type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)



parseIf :: Parser String String LAst
parseIf = do
	symbol '{'
	stringParser "#KOLI#"
    	expr <- parseLExpr
    	stringParser "#TADI#"
   	block1 <- parseBlock
    	stringParser "#PO-INOMU#"
    	block2 <- parseBlock
	symbol '}'
    	return If {cond = expr, thn = block1, els = block2}

parseWhile :: Parser String String LAst
parseWhile = do
  	symbol '{'
	stringParser "#PAKUL#"
   	expr <- parseLExpr
  	block <- parseBlock
	symbol '}'	
  	return While {cond = expr, body = block}

parseAssign :: Parser String String LAst
parseAssign = do
	symbol '{'
    	stringParser "#ZVYAZATI#"
    	ident <- parseLIdent
    	expr <- parseLExpr
	symbol '}'
   	return Assign {var = ident, expr = expr}


parseRead :: Parser String String LAst
parseRead = do
	symbol '{'
    	stringParser "#CHITATSBERESTI#"
    	ident <- parseLIdent
	symbol '}'
    	return Read {var = ident}


parseWrite :: Parser String String LAst
parseWrite = do
	symbol '{'
    	stringParser "#NAPISATNABERESTU#"
    	expr <- parseLExpr
	symbol '}'
    	return Write {expr = expr}


parseSeq :: Parser String String LAst
parseSeq = do
    	symbol '{'
	stringParser "#ROBIT#"
    	statements <- many $ parseBlock
	symbol '}'
    	return Seq {statements = statements}

parseEmpty :: Parser String String LAst
parseEmpty = do
    	symbol '{'
	stringParser "#PUSTO#"
	symbol '}'
    	return Seq {statements = []}


parseBlock :: Parser String String LAst
parseBlock = parseIf <|> parseWhile <|> parseAssign <|> parseRead <|> parseWrite <|> parseSeq <|> parseEmpty


parseL :: Parser String String LAst
parseL = parseSeq