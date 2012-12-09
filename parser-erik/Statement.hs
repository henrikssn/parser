module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Block [Statement] |
    Skip |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show 

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

if_ = accept "if" -# Expr.parse # require "then" -# parse #
      accept "else" -# parse >-> (uncurry . uncurry) If

block = accept "begin" -# iter parse #- require "end" >-> Block

skip = accept "skip" # require ";" >-> const Skip

while = accept "while" -# Expr.parse # require "do" -# parse >-> uncurry While

read = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

parseStatement = assignment ! if_ ! block ! skip ! while ! read ! write

instance Parse Statement where
  parse = parseStatement 
  toString = show 
