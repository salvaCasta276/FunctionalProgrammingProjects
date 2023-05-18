import ExprSpec
import ProgramSpec
import StatementSpec
import Test.Hspec.Runner

main :: IO ()
main = do
    e <- hspecResult exprSpec
    p <- hspecResult programSpec
    s <- hspecResult statementSpec
    print e
    print p
    print s