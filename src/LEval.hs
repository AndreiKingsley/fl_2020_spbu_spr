module LEval where

import qualified Data.Map    as Map
import LLang 
import Combinators

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program funcs main) input = eval main
	(Conf{ 
	subst = Map.empty,
	input = input,
	output = [],
	defs = (Map.fromList ((\fun@(Function name _ _ _) -> (name, fun)) <$> funcs))
	})
		

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg progStr input = case runParser parseProg progStr  of
    Success (InputStream _ _) prog -> evalProg prog input
    otherwise -> Nothing