module LLang where

import           AST         
import           Combinators
import           Expr 
import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Text.Printf (printf)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | Return { expr :: Expr }
  deriving (Eq)

parseL :: Parser String String LAst
parseL = error "parseL undefined"

parseDef :: Parser String String Function
parseDef = error "parseDef undefined"

parseProg :: Parser String String Program
parseProg = error "parseProg undefined"

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval block conf@(Conf subst input output) = case block of

	(If cond thn els) -> case evalExpr subst cond of
		Just 0 -> eval els conf
		Nothing -> Nothing
		_ -> eval thn conf

	(While cond body) -> case evalExpr subst cond of
		Nothing -> Nothing
		Just 0 -> Just conf
		_ -> case eval body conf of
			Just conf' -> eval block conf'
			_ -> Nothing

	(Assign var expr) -> case evalExpr subst expr of
		Just x -> Just $ Conf (Map.insert var x subst) input output
		_ -> Nothing

	(Read var) -> case input of
		x:xs -> Just $ Conf (Map.insert var x subst) xs output
		_ -> Nothing

	(Write expr) -> case evalExpr subst expr of
		Just x -> Just $ Conf subst input (x:output)
		_ -> Nothing

	(Seq []) -> Just conf
	(Seq (b:bs)) -> case eval b conf of
		Just conf' -> eval (Seq bs) conf'
		_ -> Nothing 

instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

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
          Return expr     -> makeIdent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
