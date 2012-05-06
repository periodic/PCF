module Language.PCF.Test where

import Language.PCF.Grammar
import Language.PCF.Parser
import Language.PCF.Text.Pretty

prop_PrintParse :: Expr -> Bool
prop_PrintParse e = either (const False) (== e) . runPCFParser "TEST" . prettyPrint $ e
