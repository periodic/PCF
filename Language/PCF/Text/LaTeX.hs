module Language.PCF.Text.LaTeX where

import Language.PCF.Grammar
import Text.Printf


latexPrint :: Expr -> String
latexPrint (Var name) = show name
latexPrint BoolTrue  = "true"
latexPrint BoolFalse = "false"
latexPrint (Nat i)   = show i
latexPrint (Eq a b)  = printf "\\eq{%s}{%s}" (latexPrint a) (latexPrint b)
latexPrint (IfThenElse p a b) = printf "\\ifthenelse{%s}{%s}{%s}" (latexPrint p) (latexPrint a) (latexPrint b)
latexPrint (Add a b) = printf "%s + %s" (latexPrint a) (latexPrint b)
-- latexPrint (Sub a b) = printf "(%s - %s)" (latexPrint a) (latexPrint b)
latexPrint (Pair a b) = printf "\\pair{%s}{%s}" (latexPrint a) (latexPrint b)
latexPrint (Proj i e) = printf "\\proj{%i}{%s}" i (latexPrint e)
latexPrint (Lambda v body) = printf "(\\fnt{%s}{%s})" (show v) (latexPrint body)
latexPrint (Ap f arg) = printf "%s %s" (latexPrint f) (latexPrint arg)
latexPrint (Fix f) = printf "(fix %s)" (latexPrint f)
latexPrint _           = "(??)"


latexCommands :: String
latexCommands = "\
\\newcommand{\\eqdef}{\\overset{def}{=}} \
\\newcommand{\\f}[3]{\\lambda #1 : #2 . \\; #3} \
\\newcommand{\\fnt}[2]{\\lambda #1 . \\; #2} \
\\newcommand{\\ftype}[2]{#1 \\rightarrow #2} \
\\newcommand{\\pair}[2]{ \\langle #1 , #2 \\rangle } \
\\newcommand{\\eq}[2]{Eq?\\;#1\\;#2} \
\\newcommand{\\proj}[2]{\\textbf{Proj_#1}\\;#2} \
\ \
\\newcommand{\\ifthenelse}[3]{\\texttt{if}\\;#1\\;\\texttt{then}\\;#2\\;\\texttt{else}\\;#3} \
\\newcommand{\\letin}[2]{\\texttt{let}\\;#1\\;\\texttt{in}\\;#2} \
\\newcommand{\\letinrec}[2]{\\texttt{letrec}\\;#1\\;\\texttt{in}\\;#2} \
\ "
