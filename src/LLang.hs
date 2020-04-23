module LLang where

import           AST         
import           Combinators
import           Expr 
import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Text.Printf (printf)
import Control.Applicative

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst } 
	deriving (Eq)

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr } 
	deriving (Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

-----------


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

parseFunctionCall :: Parser String String AST
parseFunctionCall = do
	name <- parseFunName
	args <- many $ Ident <$> parseFunArg
	return $ FunctionCall name args

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
                     (Num <$> parseLNum <|> Ident <$> parseLIdent <|> symbol '(' *> parseLExpr <* symbol ')' <|> parseFunctionCall)
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

--------

parseFunName :: Parser String String String
parseFunName = do
	symbol '_'
	s1 <- many parserus
        s2 <- many parseRUS
	symbol '_'
	return $ "_"++s1++s2++"_"

parseFunArg :: Parser String String String
parseFunArg = do
	symbol '('
	ident <- parseLIdent
	symbol ')'
	return ident

parseReturn :: Parser String String AST
parseReturn = do
	symbol '{'
	stringParser "#VIDDAI#"
	expr <- parseLExpr
	symbol '}'
    	return expr

parseDef :: Parser String String Function
parseDef = do
	symbol '{'
	stringParser "#VIZNACH#"
	name <- parseFunName
	args <- many $ parseFunArg
	body <- parseSeq
	retExpr <- parseReturn
	symbol '}'
	return $ Function name args body retExpr
	

parseProg :: Parser String String Program
parseProg = do
	stringParser "~SHUE_PPSH~"
	funcs <- many $ parseDef
	main <- parseL
	return $ Program funcs main


--------------------

evalLExpr :: Configuration -> AST -> Maybe (Configuration, Int)
evalLExpr conf (Num x)        = Just (conf, x)
evalLExpr conf (Ident str)    = case Map.lookup str (subst conf) of
	(Just x) -> Just (conf, x)
	_        -> Just (conf, 0)
evalLExpr conf (BinOp Plus x y) = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf'', a + b)
evalLExpr conf (BinOp Mult x y)  = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y  
	return (conf', a * b)
evalLExpr conf (BinOp Minus x y) = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', a - b)
evalLExpr conf (BinOp Div x y)  = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', div a b)
evalLExpr conf (BinOp Pow x y)  = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', a ^ b)
evalLExpr conf (BinOp Equal x y)  = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', bti (a == b))
evalLExpr conf (BinOp Nequal x y)   = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', bti (a /= b))
evalLExpr conf (BinOp Gt x y)  = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf',bti (a > b))
evalLExpr conf (BinOp Ge x y)  = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf',bti (a >= b))
evalLExpr conf (BinOp Lt x y)   = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', bti (a < b))
evalLExpr conf (BinOp Le x y) = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', bti (a <= b))
evalLExpr conf (BinOp And x y)= do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', bti $ (itb a) && (itb b))
evalLExpr conf (BinOp Or x y)  = do
	(conf', a)  <- evalLExpr conf  x 
	(conf'', b) <- evalLExpr conf' y 
	return (conf', bti $ (itb a) || (itb b))
evalLExpr conf (UnaryOp Not x)= do
	(conf', a)  <- evalLExpr conf  x 
	return (conf', bti $ not $ itb a)
evalLExpr conf (UnaryOp Minus x)    = do
	(conf', a)  <- evalLExpr conf  x 
	return (conf', (-1) * a)
evalLexpr conf (FunctionCall name vals)= do
	(conf', argVals) <- evalFunArgs conf vals
	func <- Map.lookup name (defs conf')
	(conf'', a) <- evalFunction func argVals conf'
	return (conf'', a)

evalFunArgs :: Configuration -> [AST] -> Maybe (Configuration, [Int])
evalFunArgs conf [] = Just(conf, [])
evalFunArgs conf (x:xs) = do
	(conf', a) <- evalLExpr conf x
	(conf'', as) <- evalFunArgs conf' xs
	return (conf'', (a:as))

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input [] Map.empty

eval :: LAst -> Configuration -> Maybe Configuration
eval block conf@(Conf subst input output defs) = case block of

	(If cond thn els) -> case evalLExpr conf cond of
		Just(conf', 0) -> eval els conf'
		Nothing -> Nothing
		Just(conf', _) -> eval thn conf'

	(While cond body) -> case evalLExpr conf cond of
		Nothing -> Nothing
		Just(conf', 0) -> Just conf'
		Just(conf', _) -> case eval body conf' of
			Just conf'' -> eval block conf''
			_ -> Nothing

	(Assign var expr) -> case evalLExpr conf expr of
		Just((Conf subst' input' output' defs'), x) -> Just $ Conf (Map.insert var x subst') input' output' defs'
		_ -> Nothing

	(Read var) -> case input of
		x:xs -> Just $ Conf (Map.insert var x subst) xs output defs
		_ -> Nothing

	(Write expr) -> case evalLExpr conf expr of
		Just((Conf subst' input' output' defs'), x) -> Just $ Conf subst' input' (x:output') defs'
		_ -> Nothing

	(Seq []) -> Just conf
	(Seq (b:bs)) -> case eval b conf of
		Just conf' -> eval (Seq bs) conf'
		_ -> Nothing 


evalFunction :: Function -> [Int] -> Configuration -> Maybe(Configuration, Int)
evalFunction (Function name args body ret) argVals conf@(Conf subst input output defs) = do
	let subst' = Map.fromList (zip args argVals)
	conf' <- eval body $ Conf subst' input output defs
	((Conf _ input' output' _), retVal) <- evalLExpr conf' ret 
	return ((Conf subst input' output' defs), retVal)

instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
