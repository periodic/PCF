{-# LANGUAGE OverloadedStrings #-}
module Language.PCF.Text.Html where

import Prelude hiding (head, id, div, span)

import Language.PCF.Grammar
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span)

instance ToHtml Expr where
    toHtml (Var i)      = span ! class_ "var" $ toHtml (show i)

    toHtml (BoolTrue)   = span ! class_ "bool" $ "true"

    toHtml (BoolFalse)  = span ! class_ "bool" $ "false"

    toHtml (Nat i)      = span ! class_ "nat" $ toHtml (show i)

    toHtml (Eq          a b)    = span ! class_ "eq bool" $ do
        keyword $ "Eq?"
        toHtml a
        toHtml b

    toHtml (IfThenElse  p t f)  = span ! class_ "conditional" $ do
        keyword $ "if"
        toHtml p
        keyword $ "then"
        toHtml t
        keyword $ "else"
        toHtml f

    toHtml (Add         a b)    = span ! class_ "nat" $ do
        toHtml a
        span ! class_ "operator" $ "+"
        toHtml b

    toHtml (Pair        a b)    = span ! class_ "pair" $ do
        "<"
        toHtml a
        ","
        toHtml b
        ">"

    toHtml (Proj        i e)    = span ! class_ "" $ do
        keyword $ do
            "Proj"
            sub . toHtml $ show i
        toHtml e

    toHtml (Lambda      i e)    = span ! class_ "lambda" $ do
        span ! class_ "operator" $ "λ"
        toHtml $ show i
        "."
        toHtml e

    toHtml (Ap          a b)    = span ! class_ "" $ do
        toHtml a
        "("
        toHtml b
        ")"
    toHtml (Fix         f)      = span ! class_ "lambda" $ do
        keyword "fix"
        toHtml f

keyword = span ! class_ "keyword"
