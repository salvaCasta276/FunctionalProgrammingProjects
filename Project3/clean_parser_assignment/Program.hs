module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T]
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program []) = []
  toString (Program (x:xs)) = Statement.toString x ++ toString (Program xs) 
             
exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty
