module Expr where

import           AST                 
import           Combinators         
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit)
import qualified Data.Map            as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr subst (Num x)        = Just x
evalExpr subst (Ident str)    = case Map.lookup str subst of
	a@(Just x) -> a
	_          -> Just 0
evalExpr subst (BinOp Plus x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ a + b
evalExpr subst (BinOp Mult x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ a * b
evalExpr subst (BinOp Minus x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ a - b
evalExpr subst (BinOp Div x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ div a b
evalExpr subst (BinOp Pow x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ a ^ b
evalExpr subst (BinOp Equal x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti (a == b)
evalExpr subst (BinOp Nequal x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti (a /= b)
evalExpr subst (BinOp Gt x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti (a > b)
evalExpr subst (BinOp Ge x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti (a >= b)
evalExpr subst (BinOp Lt x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti (a < b)
evalExpr subst (BinOp Le x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti (a <= b)
evalExpr subst (BinOp And x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti $ (itb a) && 	(itb b)
evalExpr subst (BinOp Or x y)     = do
	a <- evalExpr subst x
	b <- evalExpr subst y
	return $ bti $ (itb a) ||  (itb b)
evalExpr subst (UnaryOp Not x)        = do
	a <- evalExpr subst x
	return $ bti $ not $ itb a
evalExpr subst (UnaryOp Minus x)        = do
	a <- evalExpr subst x
	return $ (-1) * a

bti :: Bool -> Int
bti True = 1
bti False = 0

itb :: Int -> Bool
itb 0 = False
itb _ = True

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr ((p, assoc):xs) epsPars doBin doUn=
  let rest = uberExpr xs epsPars doBin doUn
  in case assoc of
    Unary             -> doUn <$> p <*> rest <|> rest
    Binary LeftAssoc  -> do
        (first, last) <- (,) <$> rest <*> (many ((,) <$> p <*> rest))
        return $ foldl (\acc (op, ast) -> doBin op acc ast) first last
      <|> rest
    Binary RightAssoc -> do
        (first, last) <- (,) <$> (many ((,) <$> rest <*> p)) <*> rest
        return $ foldr (\(ast, op) acc -> doBin op ast acc) last first
      <|> rest
    Binary NoAssoc    -> do
        ast1 <- rest
        op <- p
        ast2 <- rest
        return $ doBin op ast1 ast2
      <|> rest
uberExpr [] epsPars _ _ = epsPars


-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [
                      (orParser, Binary RightAssoc),
                      (andParser, Binary RightAssoc),
                      (notParser, Unary),
                      (equalParser <|> nequalParser <|> leParser <|> geParser <|> gtParser <|> ltParser, Binary NoAssoc),
                      (plusParser <|> minusParser, Binary LeftAssoc),
                      (multParser <|> divParser, Binary LeftAssoc),
		      (minusParser, Unary),
                      (powParser, Binary RightAssoc)
                     ]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
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

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

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

opList = ["+", "*", "-", "/", "^", "==", "/=", ">", ">=", "<", "<=", "&&", "||", "!"]

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
toOperator "!" = return Not
toOperator _   = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null (stream rest) -> return $ compute ast
    _                                     -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = (compute x) ^ (compute y)

