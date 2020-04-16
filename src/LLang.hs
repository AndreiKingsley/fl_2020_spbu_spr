module LLang where

import AST 
import Combinators
import Control.Applicative
import Expr
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

parseL :: Parser String String LAst
parseL = error "parseL undefined"

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
		