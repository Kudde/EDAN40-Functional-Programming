module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement = Assignment String Expr.T
        | If            Expr.T Statement Statement
        | While         Expr.T Statement
        | Begin         [Statement]
        | Write         Expr.T
        | Read          String
        | Comment       String
        | Skip
        deriving Show

-- Example program
-- read k;
-- read n;
-- m := 1;
-- while n-m do
--   begin
--     if m - m/k*k then
--       skip;
--     else
--       write m;
--     m := m + 1;
--   end

-- Grammar
-- program ::= statements
--   statement ::= variable ':=' expr ';'
--           | 'skip' ';'
--           | 'begin' statements 'end'
--           | 'if' expr 'then' statement 'else' statement
--           | 'while' expr 'do' statement
--           | 'read' variable ';'
--           | 'write' expr ';'
--   statements ::= {statement}
--   variable ::= letter {letter}


assStmt = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skipStmt = accept "skip" # require ";" >-> buildSkip
buildSkip ss = Skip

beginStmt = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin ss = Begin ss

ifStmt = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

whileStmt = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
buildWhile (e, s) = While e s

readStmt = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

writeStmt = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

commentStmt = accept "--" -# line #- require "\n" >-> buildComment
buildComment s = Comment s


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input                              = []
exec (Assignment name expr : stmts) dict input  = exec stmts (Dictionary.insert (name, Expr.value expr dict) dict) input
exec (Skip : stmts) dict input                  = exec stmts dict input
exec (Begin stmt : stmts) dict input            = exec (stmt ++ stmts) dict input
exec (Read v : stmts) dict input                = exec stmts (Dictionary.insert (v, head input) dict) (tail input)
exec (Write expr : stmts) dict input            = Expr.value expr dict : exec stmts dict input
exec (Comment stmt : stmts) dict input          = exec stmts dict input

exec (If cond thenStmts elseStmts : stmts) dict input =
        if   Expr.value cond dict > 0
        then exec (thenStmts : stmts) dict input
        else exec (elseStmts : stmts) dict input

exec (While cond doStmt : stmts) dict input =
        if   Expr.value cond dict > 0
        then exec (doStmt : While cond doStmt : stmts) dict input
        else exec stmts dict input

tabLength = 2
tab n = replicate (n*tabLength) ' '

shw :: Int -> Statement -> String
shw n (Assignment name expr)    = tab n ++ name      ++ " := " ++ Expr.toString expr ++ ";\n"
shw n (Begin stmt)              = tab n ++ "begin\n" ++ concatMap (shw (n + 1)) stmt ++ tab n ++ "end\n"
shw n (Read v)                  = tab n ++ "read "   ++ v ++ ";\n"
shw n (Write expr)              = tab n ++ "write "  ++ Expr.toString expr ++ ";\n"
shw n (Comment s)               = tab n ++ "--"      ++ s ++ "\n"
shw n (While cond stmt)         = tab n ++ "while "  ++ Expr.toString cond ++ " do\n" ++ shw (n + 1) stmt
shw n (Skip)                    = tab n ++ "skip;\n"

shw n (If cond thenStmts elseStmt) = tab n ++ "if " ++ Expr.toString cond ++ " then\n"
                                  ++ shw (n + 1) thenStmts ++ tab n ++ "else\n"
                                  ++ shw (n + 1) elseStmt



instance Parse Statement where
        parse = assStmt ! skipStmt ! beginStmt ! ifStmt ! whileStmt ! readStmt ! writeStmt ! commentStmt
        toString = shw 0
