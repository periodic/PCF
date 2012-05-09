module Language.PCF.Test where

import Language.PCF.Grammar
import Language.PCF.Parser
import Language.PCF.Text.Pretty

prop_PrintParse :: ExprWCtx -> Bool
prop_PrintParse e =
    let s1 = prettyPrint e
        p1 = runPCFParser "TEST1" s1
     in case p1 of
        Left _ -> False
        Right e2 -> s1 == prettyPrint e2
