module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Pow Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr, pow :: Parser Expr

term', expr', pow' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\_ -> Mul) !
        lit '/' >-> (\_ -> Div)

addOp = lit '+' >-> (\_ -> Add) !
        lit '-' >-> (\_ -> Sub)

powOp = lit '^' >-> (\_ -> Pow)

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

pow' e = powOp # pow >-> bldOp e ! return e
pow = factor #> pow'
             
term' e = mulOp # pow >-> bldOp e #> term' ! return e
term = pow #> term'
       
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Pow t u) = parens (prec>7) (shw 8 t ++ "^" ++ shw 7 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var v) dict =
        case Dictionary.lookup v dict of
        Just n -> n
        Nothing -> error ("undefined variable " ++ (show v))
value (Add e1 e2) dict = value e1 dict + value e2 dict
value (Sub e1 e2) dict = value e1 dict - value e2 dict
value (Mul e1 e2) dict = value e1 dict * value e2 dict
value (Pow e1 e2) dict = value e1 dict ^ value e2 dict
value (Div e1 e2) dict =
        case value e2 dict of
        0 -> error ("division by 0")
        _ -> value e1 dict `div` value e2 dict

instance Parse Expr where
    parse = expr
    toString = shw 0
