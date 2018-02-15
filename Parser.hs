module Main (main) where

import MiniLex
import System.Environment

{-
=============================================================================
== The Context Free Grammar supplied by Dr. Cockett required transformations
== before it could be used. For the details of making the grammar a LL(1)
== grammar and a recursive descent grammar please look at README.md.
============================================================================= 
== Grammar to implement:
=============================================================================
stmt -> IF expr thenpart elsepart       R1
      | WHILE expr dopart               R2
      | INPUT ID                        R3
      | ID ASSIGN expr                  R4
      | WRITE expr                      R5
      | BEGIN stmtlist endpart.         R6
thenpart -> THEN stmt.                  R7
elsepart -> ELSE stmt.                  R8
dopart -> DO stmt.                      R9
endpart -> END.                         R10
stmtlist -> stmt semipart               R11
          | .                           R12
semipart -> SEMICOLON stmtlist.         R13
expr -> term expr'.                     R14
expr' -> ADD term expr'                 R15
       | SUB term expr'                 R16
       | .                              R17
term -> factor term'.                   R18
term' -> MUL factor term'               R19
       | DIV factor term'               R20
       | .                              R21
factor -> LPAR expr clopar              R22
        | ID                            R23
        | NUM                           R24
        | SUB NUM.                      R25
clopar -> RPAR.                         R26

-}


data Stmt = If Exp Stmt Stmt
            |While Exp Stmt
            |Assign String Exp
            |Block [Stmt]
            |Print Exp
            |Input Exp

data Exp = Add Exp Exp
            |Mul Exp Exp
            |Div Exp Exp
            |Neg Exp
            |Id String
            |Num Integer

stmt :: [TokenPos] -> ([TokenPos], Stmt)
stmt ((TUP(t, p)):ts) = case t of
    TWHILE -> (\(r, e) -> (\(r', s) -> (r', While e s)) $ dopart r) $ expr ts
    TINPUT -> (\((TUP(TID str, _)):ts') -> (ts', Input $ Id str)) ts
    TID v  -> (\((TUP(TASSIGN, _)):ts') -> (\(r, e) -> (r, Assign v e))  $ expr ts') ts
    TWRITE -> (\(r, e) -> (r, Print e)) $ expr ts
    TBEGIN -> (\(r, ss) -> (\r' -> (r', Block ss)) $ endpart r) $ stmtlist ts 
    TIF -> (\(r, e) -> (\(r', s) -> (\(r'',s') -> (r'', If e s s')) $ elsepart r') $ thenpart r) $ expr ts
    _ -> stmt ts -- Error

thenpart :: [TokenPos] -> ([TokenPos], Stmt)
thenpart ((TUP(TTHEN, _)):ts) = stmt ts
thenpart (t:ts) = error $ "Expected: 'Then' statement\nActual: " ++ (show t) ++ "\n"
thepart [] = error "Nothing found"

elsepart :: [TokenPos] -> ([TokenPos], Stmt)
elsepart ((TUP(TELSE, _)):ts) = stmt ts

dopart :: [TokenPos] -> ([TokenPos], Stmt)
dopart ((TUP(TDO, _)):ts) = stmt ts

endpart :: [TokenPos] -> [TokenPos]
endpart ((TUP(TEND, _)):ts) = ts

stmtlist :: [TokenPos] -> ([TokenPos], [Stmt])
stmtlist ts = (\(r, s) -> (\(r',ss) -> (r', ([s] ++ ss)) ) $ semipart r) $ stmt ts
--R11 stmtlist ts = ts -- TODO: Epsilon

semipart :: [TokenPos] -> ([TokenPos], [Stmt])
semipart ((TUP(TSEMICOLON, _)):ts) = stmtlist ts

expr :: [TokenPos] -> ([TokenPos], Exp)
expr ts = (\(r, s) -> expr' r) $ term ts

expr' ((TUP(t, p)):ts) = case t of
    TADD -> (\(r, e) -> (\(r', e') -> (r', Add e e')) $ expr' r) $ term ts
    TSUB -> (\(r, e) -> (\(r', e') -> (r', Add e (Neg e'))) $ expr' r) $ term ts
    _ -> expr' ts -- TODO: Epsilon

term ts = (\(r, e) ->  term' r) $ factor ts

term' ((TUP(t, p)):ts) = case t of
    TMUL -> (\(r, e) -> (\(r', e') -> (r', Mul e e')) $ term' r) $ factor ts
    TDIV -> (\(r, e) -> (\(r', e') -> (r', Div e e')) $ term' r) $ factor ts
    _ -> term' ts -- TODO: Epsilon

factor :: [TokenPos] -> ([TokenPos], Exp)
factor ((TUP(t, p)):ts) = case t of
    TLPAR -> (\(r, e) -> ((clopar r), e)) $ expr ts
    TID v -> (ts, Id v)
    TNUM n -> (ts, Num (toInteger n))
    TSUB -> (\(TUP((TNUM n), _):ts') -> (ts', Neg (Num (toInteger n)))) ts


clopar :: [TokenPos] -> [TokenPos]
clopar ((TUP(TRPAR, _)):ts) = ts


main = do
    file <- getArgs
    s <-readFile (file !! 0)
    let toks = lexer s
    print $ show toks

--    case toks of
--        Right ts -> print $ show ts
--        Left str -> error str


test0 = lexer "input x"
