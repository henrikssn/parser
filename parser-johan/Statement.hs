module Statement--(T, parse, toString, fromString, exec) 
where

import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement = Assignment String Expr.T
               | Skip
               | BeginEnd     [Statement]
               | IfElse Expr.T Statement Statement
               | While  Expr.T Statement
               | Read   String
               | Write  Expr.T
               | Empty
               deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" 
             >-> uncurry Assignment

skip :: Parser Statement
skip = (accept "skip" # require ";") 
       -# return Skip

beginEnd :: Parser Statement
beginEnd = accept "begin" -# iter parse #- require "end" 
           >-> BeginEnd

ifElse :: Parser Statement
ifElse =   (accept "if"    -# Expr.parse) 
         # (require "then" -# statementParse)
         # (accept  "else" -# statementParse ! return Empty)
         >-> (uncurry . uncurry) IfElse

while :: Parser Statement
while =   (accept  "while" -# Expr.parse)
        # (require "do"    -# statementParse)
        >-> uncurry While

read_ :: Parser Statement
read_ = accept "read" -# word #- require ";"
        >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";"
        >-> Write

statementParse :: Parser Statement
statementParse = assignment ! skip ! beginEnd ! ifElse ! while ! read_ ! write

exec (IfElse cond thenStmts elseStmts: stmts) dict input = 
    if   (Expr.value cond dict) > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse    = statementParse
  toString = show
