module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip | While Expr.T BegEnd |
    Read String | Write Expr.T |
    BegEnd [Statement]
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifElse = accept "if" -# Expr.parse #- require "then" # Statement.parse #- require "else" # Statement.parse >-> buildIfElse
buildIfElse (cond, thenStmts, elseStmts) = If cond thenStmts elseStmts

while = accept "while" -# Expr.parse #- require "do" # Statement.parse >-> buildWhile
buildWhile expr loopedStmt = While expr beg 

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

read = accept "read" -# word #- require ";" >-> buildRead
buildRead var = Read var

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite expr = Write expr 

begEnd = accept "begin" -# iter (Statement.parser ! skip) #- require "end" >-> buildBegEnd
begEnd [stmts] = BegEnd [stmts]


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment varName valueExpr : stmts) dict input =
    exec stmts (Dictionary.insert (varName, Expr.value valueExpr dict) dict) input
exec (While expr loopedStmt : stmts) dict input
    | Expr.value expr dict > 0 = exec (loopedStmt : While expr loopedStmt : stmts) dict input
    | otherwise = exec stmts dict input
exec (Read var : stmts) dict [] = error ("No input value for read variable" ++ (show var))
exec (Read var : stmts) dict (x:xs) = exec stmts (Dictionary.insert (var, x) dict xs)
exec (Write expr : stmts) dict input = (Expr.value expr dict : exec stmts dict input)
exec (BegEnd (Skip : skippedStmts) : stmts) dict input = exec stmts dict input
exec (BegEnd (x:xs) : stmts) dict input = exec (x : BegEnd xs : stmts) dict input
exec (BegEnd [] : stmts) dict input = exec stmts dict input


instance Parse Statement where
  parse = assignment ! ifElse ! while ! read ! write
  toString = error "Statement.toString not implemented"
