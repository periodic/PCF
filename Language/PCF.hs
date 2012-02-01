module Language.PCF ( module Language.PCF.Grammar
                    , module Language.PCF.Parser
                    , module Language.PCF.Eval
                    ) where

import Language.PCF.Grammar
import Language.PCF.Parser
import Language.PCF.Eval
import Language.PCF.Text.Pretty


explain :: String -> String -> IO ()
explain name input = case runPCFParser name input of
                       Left err     -> print err
                       Right expr   -> mapM_ (putStrLn . prettyPrint) $ showSteps expr

