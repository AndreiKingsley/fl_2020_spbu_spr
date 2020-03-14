module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail',
                              	 satisfy, symbol)
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
parseExpr = uberExpr
		[(sumPars <|> minPars, LeftAssoc), (mulPars <|> divPars, LeftAssoc), (powPars, RightAssoc)]
		(Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')') 
		BinOp
	where 
		sumPars = symbol '+' >>= toOperator
		minPars = symbol '-' >>= toOperator
		mulPars = symbol '*' >>= toOperator
		divPars = symbol '/' >>= toOperator
		powPars = symbol '^' >>= toOperator

-- Парсер для натуральных чисел с 0
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator '^' = return Pow
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

