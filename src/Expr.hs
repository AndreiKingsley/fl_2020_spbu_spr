module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail',
                              	 satisfy, symbol, stringParser, dictParser)
import           Data.Char   (digitToInt, isDigit)
import 		 Control.Applicative

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr ((p, assoc):xs) epsPars doAst = 
	let rest = uberExpr xs epsPars doAst
  	in case assoc of
		LeftAssoc  -> do
    	 		(first, arr) <- (,) <$> rest <*> (many ((,) <$> p <*> rest))
   			return $ foldl (\acc (op, ast) -> doAst op acc ast) first arr
  		RightAssoc -> do
     		  	(arr, last) <- (,) <$> (many ((,) <$> rest <*> p)) <*> rest
      			return $ foldr (\(ast, op) acc -> doAst op ast acc) last arr
		NoAssoc    -> (do
			ast1 <- rest
			op   <- p
     			ast2 <- rest
                        return (doAst op ast1 ast2)) <|> rest
uberExpr [] epsPars _ = epsPars

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [
                      (orParser, RightAssoc),
                      (andParser, RightAssoc),
                      (equalParser <|> nequalParser <|> leParser <|> geParser <|> gtParser <|> ltParser, NoAssoc),
                      (plusParser <|> minusParser, LeftAssoc),
                      (multParser <|> divParser, LeftAssoc),
                      (powParser, RightAssoc)
                     ]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp
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
	
		


-- Старый парсер без минуса
parseNum' :: Parser String String Int
parseNum' = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)
	
-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = Parser $ \input ->
    case input of
        ('-':xs) -> case runParser parseNum xs of
                    Success i r -> Success i ((-1) * r)
                    Failure e -> Failure e 
        _        -> runParser parseNum' input
                   
	
parseEngLetter :: Parser String String Char
parseEngLetter = satisfy $ \x -> elem x (['a'..'z'] ++ ['A'..'Z']) 	

parseDigit :: Parser String String Char
parseDigit = satisfy isDigit

parseUnderscore :: Parser String String Char
parseUnderscore = satisfy (== '_')
		
parseIdent :: Parser String String String
parseIdent = do
	firstSymbol <- parseEngLetter <|> parseUnderscore
	rest <- many $ parseEngLetter <|> parseUnderscore <|> parseDigit
	return $ firstSymbol : rest

opList = ["+", "*", "-", "/", "^", "==", "/=", ">", ">=", "<", "<=", "&&", "||"]

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = dictParser opList >>= toOperator

-- Преобразование строк операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+" = return Plus
toOperator "*" = return Mult
toOperator "-" = return Minus
toOperator "/" = return Div
toOperator "^" = return Pow
toOperator "==" = return Equal
toOperator "/=" = return Nequal
toOperator ">" = return Gt
toOperator ">=" = return Ge
toOperator "<" = return Lt
toOperator "<=" = return Le
toOperator "&&" = return And
toOperator "||" = return Or
toOperator _   = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = (compute x) ^ (compute y)

