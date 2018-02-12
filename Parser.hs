{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import MiniLexer
import System.Environment

data Prog = R1 Stmt
        deriving (Show, Eq)

data Stmt = R2 PIf Expr PThen Stmt PElse Stmt
           |R3 PWhile Expr PDo Stmt
           |R4 PInput PId
           |R5 PId PAssign Expr
           |R6 PWrite Expr
           |R7 PBegin Stmtlist PEnd
        deriving (Show, Eq)

data Then = R2' PThen Stmt Else

data Else = R2'' PElse Stmt

data Addop = R8 PAdd
            |R9 PSub
        deriving (Show, Eq)
data Mulop = R10 PMul
            |R11 PDiv
        deriving (Show, Eq)
data Factor = R12 PLpar Expr PRpar
             |R13 PId
             |R14 PNum
             |R15 PSub PNum
        deriving (Show, Eq)

data Stmtlist = R16 Stmtlist'
        deriving (Show, Eq)
data Stmtlist' = R17 Stmt PSemicolon Stmtlist'
                |R18
        deriving (Show, Eq)
data Expr = R19 Term Expr'
        deriving (Show, Eq)
data Expr' = R20 Addop Term Expr'
            |R21
        deriving (Show, Eq)
data Term = R22 Factor Term'
        deriving (Show, Eq)
data Term' = R23 Mulop Factor Term'
            |R24
        deriving (Show, Eq)



data PIf = PIF
        deriving (Show, Eq)
data PThen = PTHEN
        deriving (Show, Eq)
data PWhile = PWHILE
        deriving (Show, Eq)
data PDo = PDO
        deriving (Show, Eq)
data PInput = PINPUT
        deriving (Show, Eq)
data PElse = PELSE
        deriving (Show, Eq)
data PBegin = PBEGIN
        deriving (Show, Eq)
data PEnd = PEND
        deriving (Show, Eq)
data PWrite = PWRITE
        deriving (Show, Eq)
data PId = PID [Char]
        deriving (Show, Eq)
data PNum = PNUM Int
        deriving (Show, Eq)
data PAssign = PASSIGN
        deriving (Show, Eq)
data PAdd = PADD
        deriving (Show, Eq)
data PSub = PSUB
        deriving (Show, Eq)
data PMul = PMUL
        deriving (Show, Eq)
data PDiv = PDIV
        deriving (Show, Eq)
data PLpar = PLPAR
        deriving (Show, Eq)
data PRpar = PRPAR
        deriving (Show, Eq)
data PSemicolon = PSEMICOLON
        deriving (Show, Eq)


prog :: [Token] -> Either [Char] ([Token], Prog)
prog ts = case stmt ts of
    Right (ts, stm) -> case ts of
        [] -> Right (ts, R1 stm)
        otherwise  -> Left $ "Tokens remain: " ++ (show ts)

stmt :: [Token] -> Either [Char] ([Token], Stmt)
stmt (t:ts) = case t of
    TIF p -> stmt ts
    TWHILE p -> stmt ts
    TINPUT p -> case ts of
        ((TID v p):ts') -> Right (ts', R4 PINPUT (PID v))
        _ -> Left "Invalid pattern R4: ID not found"
    TID v p -> case ts of
        ((TASSIGN p):ts') -> case expr ts' of
            Right (ts'', exp) -> Right (ts'', R5 (PID v) PASSIGN exp)
            Left msg -> Left $ "Invalid match R5: " ++ msg
        _ -> Left "Invalid match R5: Assign not found"
    TWRITE p -> case expr ts of
        Right (ts', exp) -> Right (ts', R6 PWRITE exp)
        Left msg -> Left $ "Invalid pattern R6: " ++ msg
    TBEGIN p -> case stmtlist ts of
        Right (ts', smtls) -> case ts' of
            ((TEND p):ts'') -> Right (ts'', R7 PBEGIN smtls PEND)
            _ -> Left "Invalid pattern R7: end not found"
        Left msg -> Left $ "Invalid pattern R7: " ++ msg

ifelse :: [Token] -> ([Token], Else)
ifelse ts = ifelse ts

ifthen :: [Token] -> ([Token], Then)
ifthen ts = ifthen ts


addop :: [Token] -> Either [Char] ([Token], Addop)
addop (t:ts) = case t of
        TADD p  -> Right (ts, R8 PADD)
        TSUB p  -> Right (ts, R9 PSUB)
        _       -> Left "Invalid Pattern Addop"

mulop :: [Token] -> Either [Char] ([Token], Mulop)
mulop (t:ts) = case t of
        TMUL p  -> Right (ts, R10 PMUL)
        TDIV p  -> Right (ts, R11 PDIV)
        _       -> Left "Invalid Pattern Mulop"

factor :: [Token] -> Either [Char] ([Token], Factor)
factor (t:ts) = case t of
    TLPAR p -> case expr ts of
        Right ((t':ts'), pt) -> case t' of
                TRPAR p -> Right (ts', R12 PLPAR pt PRPAR)
                _       -> Left "Invalid Match on R12, With Valid Expr"
        Left _  -> Left "Invalid Match on R12, Without Valid Expr"
    TID var p   -> Right (ts, R13 (PID var))
    TNUM n p    -> Right (ts, R14 (PNUM n))
    TSUB p      -> case (\(t':ts') -> (t', ts')) ts of
                    (TNUM n p, ts') -> Right (ts', R15 PSUB (PNUM n))
                    _        -> Left "Invalid Pattern  "
    _           -> Left ("Invalid Pattern Factor")
        where
            cloPar :: [Token] -> Either String [Token]
            cloPar (t:ts) = case t of
                    TRPAR p -> Right ts
                    _       -> Left "Invalid Pattern cloPar"


stmtlist :: [Token] -> Either [Char] ([Token], Stmtlist)
stmtlist ts = case stmtlist' ts of
    Right (ts', stls) -> Right (ts', R16 stls)
    Left msg -> Left msg

stmtlist' :: [Token] -> Either [Char] ([Token], Stmtlist')
stmtlist' ts = case stmt ts of
    Right ((t:ts'), smt) -> case t of
        TSEMICOLON p -> case stmtlist' ts' of
            Right (ts'', smtls') -> Right (ts'', R17 smt PSEMICOLON smtls')
            Left msg -> Right (ts, R18)
        _ -> Right (ts, R18)
    Left msg -> Right (ts, R18)

expr :: [Token] -> Either [Char] ([Token], Expr)
expr ts = case expr' ts of
    Right (ts', exp) -> case term ts' of
        Right (ts'', trm) -> Right (ts'', R19 trm exp)
        Left msg -> Left msg
    Left msg -> Left msg


expr' :: [Token] -> Either [Char] ([Token], Expr')
expr' ts = case addop ts of
    Right (ts', adp) -> case term ts' of
        Right (ts'', trm) -> case expr' ts'' of
            Right (ts''', exp') -> Right (ts''', R20 adp trm exp')
            Left msg -> Right (ts, R21)
        Left msg -> Right (ts, R21)
    Left msg -> Right (ts, R21)


term :: [Token] -> Either [Char] ([Token], Term)
term ts = case factor ts of
    Right (ts', fct) -> case term' ts' of
        Right (ts'', trm') -> Right (ts'', R22 fct trm')
        Left msg -> Left msg
    Left msg -> Left msg

term' :: [Token] -> Either [Char] ([Token], Term')
term' ts = case mulop ts of
    Right (ts', mulp) -> case factor ts' of
        Right (ts'', fct) -> case term' ts'' of
            Right (ts''', trm') -> Right (ts''', R23 mulp fct trm')
            Left msg -> Right (ts, R24)
        Left msg -> Right (ts, R24)
    Left msg -> Right (ts, R24)







main = do
    file <- getArgs
    s <-readFile (file !! 0)
    let toks = lexer s
    case toks of
        Right ts -> print $ factor ts
        Left str -> error str


test0 = lexer "input x"
