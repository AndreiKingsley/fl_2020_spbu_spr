module LEval where

import qualified Data.Map    as Map
import LLang 

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program funcs main) input = eval main
	(Conf{ 
	subst = Map.empty,
	input = input,
	output = [],
	defs = (Map.fromList ((\fun@(Function name _ _ _) -> (name, fun)) <$> funcs))
	})
		

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg = error "parseAndEvalProg not implemented"