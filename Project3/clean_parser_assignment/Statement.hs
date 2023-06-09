--Salvador Castagnino, Aden McCusker
module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip | While Expr.T Statement |
    Read String | Write Expr.T |
    BegEnd [Statement]
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifElse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfElse
buildIfElse ((cond, thenStmts), elseStmts) = If cond thenStmts elseStmts

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (cond, loopedStmts) = While cond loopedStmts 

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

read = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write 

begEnd = accept "begin" -# iter (parse ! skip) #- require "end" >-> buildBegEnd
buildBegEnd = BegEnd

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts : stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment varName valueExpr : stmts) dict input =
    exec stmts (Dictionary.insert (varName, Expr.value valueExpr dict) dict) input
exec (While cond loopedStmts : stmts) dict input
    | Expr.value cond dict > 0 = exec (loopedStmts : While cond loopedStmts : stmts) dict input
    | otherwise = exec stmts dict input
exec (Read var : stmts) dict [] = error ("No input value for read variable" ++ show var)
exec (Read var : stmts) dict (x:xs) = exec stmts (Dictionary.insert (var, x) dict) xs
exec (Write expr : stmts) dict input = Expr.value expr dict : exec stmts dict input
exec (BegEnd (Skip : skippedStmts) : stmts) dict input = exec stmts dict input
exec (BegEnd (x:xs) : stmts) dict input = exec (x : BegEnd xs : stmts) dict input
exec (BegEnd [] : stmts) dict input = exec stmts dict input
exec (Skip : stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = ifElse ! while ! Statement.read ! write ! begEnd ! skip ! assignment

  toString (Assignment varName valueExpr) =
    varName ++ " := " ++ Expr.toString valueExpr ++ ";\n"
  toString (If cond thenStmts elseStmts) =
    "if " ++ Expr.toString cond ++ " then\n" ++ toString thenStmts ++ "else\n" ++ toString elseStmts
  toString (While cond loopedStmts) =
    "while " ++ Expr.toString cond ++ " do\n" ++ toString loopedStmts
  toString (Read var) =
    "read " ++ var ++ ";\n"
  toString (Write var) =
    "write " ++ Expr.toString var ++ ";\n"
  toString (BegEnd stmts) =
    "begin\n" ++ concatMap toString stmts ++ "end\n"
  toString Skip =
    "skip;\n"
