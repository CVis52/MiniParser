{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import MiniLexer
import System.Environment
import Data.Data
-----------------------------------------------------------------------------
-- Parser for the M- language
-----------------------------------------------------------------------------
-- Grammar to be implemented:
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
--R1   prog        -> stmt
--
-- **R2   stmt        -> IF expr THEN stmt ELSE stmt
-- **R3                 |WHILE expr DO stmt
--R4                 |INPUT ID
--R5                 |ID ASSIGN expr
--R6                 |WRITE expr
--R7                 |BEGIN stmtlist END.
--
--R2'  then -> THEN stmt else
--R2'' else -> ELSE stmt
--
--R3'  while -> WHILE expr
--R3'' do -> DO stmt
--
--
-- **stmtlist    -> stmtlist stmt SEMICOLON
-- **              |.
--
-- **expr        -> expr addop term
-- **              |term.
--
--R8   addop       -> ADD
--R9                 |SUB.
--
-- **term        -> term mulop factor
-- **              |factor.
--
--R10  mulop       -> MUL
--R11                |DIV.
--
--R12  factor      -> LPAR expr RPAR
--R13                |ID
--R14                |NUM
--R15                |SUB NUM.
--
-----------------------------------------------------------------------------
--  With the following modifications, we can produce an LL(1) grammar:
-----------------------------------------------------------------------------
--R16  stmtlist     -> stmtlist'
--R17  stmtlist'    -> stmt SEMICOLON stmtlist'
--R18                 |.
--
--R19  expr         -> term expr'.
--R20  expr'        -> addop term expr'
--R21                 |.
--
--R22  term         -> factor term'.
--R23  term'        -> mulop factor term'
--R24                 |.
-----------------------------------------------------------------------------

data Prog = R1 Stmt
        deriving (Show, Eq)
--R2 PIf Expr PThen Stmt PElse Stmt
--R3 PWhile Expr PDo Stmt
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


--discardNext target (t:ts) = 
-- Tokens with extra content: TID TNUM
prog :: [Token] -> Either [Char] ([Token], Prog)
prog ts = prog ts

stmt :: [Token] -> Either [Char] ([Token], Stmt)
stmt = stmt

addop :: [Token] -> Either [Char] ([Token], Addop)
addop (t:ts) = case t of
        TADD p  -> Right (ts, R8 PADD)
        TSUB p  -> Right (ts, R9 PSUB)
        _       -> error "Invalid Pattern Addop"

mulop :: [Token] -> Either [Char] ([Token], Mulop)
mulop (t:ts) = case t of
        TMUL p  -> Right (ts, R10 PMUL)
        TDIV p  -> Right (ts, R11 PDIV)
        _       -> error "Invalid Pattern Mulop"

--Right (\(ts',pt) -> ((cloPar ts'), R12 PLAR pt PRPAR)) $ expr ts
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

--    TSUB p      -> (\(n, ts') -> Right (ts', R15 PSUB (PNUM n))) $ getNum ts
    _           -> Left ("Invalid Pattern Factor")--error "Invalid Pattern Factor"
        where
            cloPar :: [Token] -> [Token]
            cloPar (t:ts) = case t of
                    TRPAR p -> ts
                    _       -> error "Invalid Pattern cloPar"

getNum :: [Token] -> (Int, [Token])
getNum (t:ts) = case t of
    TNUM n p -> (n, ts)
    _        -> error "Invalid Pattern getNum"

stmtlist :: [Token] -> Either [Char] ([Token], Stmtlist)
stmtlist = stmtlist

stmtlist' :: [Token] -> Either [Char] ([Token], Stmtlist')
stmtlist' = stmtlist'

{-
expr ts = Right (ts'', R19 trm exp)
    where 
        Right (ts', exp) = expr' ts
        Right (ts'', trm) = term ts'
-}
expr :: [Token] -> Either [Char] ([Token], Expr)
expr ts = case expr' ts of
    Right (ts', exp) -> case term ts' of
        Right (ts'', trm) -> Right (ts'', R19 trm exp)
        Left msg -> Left msg
    Left msg -> Left msg
{-
expr' ts = Right (ts''', R20 adp trm exp')
    where
        Right (ts', adp) = addop ts
        Right (ts'', trm) = term ts'
        Right (ts''', exp') = expr' ts''
-}
expr' :: [Token] -> Either [Char] ([Token], Expr')
expr' ts = case addop ts of
    Right (ts', adp) -> case term ts' of
        Right (ts'', trm) -> case expr' ts'' of
            Right (ts''', exp') -> Right (ts''', R20 adp trm exp')
            Left msg -> Left msg
        Left msg -> Left msg
    Left msg -> Left msg

{-
term ts = Right (ts'', R22 fct trm')
    where
        Right (ts', fct) = factor ts
        Right (ts'', trm') = term' ts'
-}
term :: [Token] -> Either [Char] ([Token], Term)
term ts = case factor ts of
    Right (ts', fct) -> case term' ts' of
        Right (ts'', trm') -> Right (ts'', R22 fct trm')
        Left msg -> Left msg
    Left msg -> Left msg

{-
term' ts = Right (ts''', R23 mulp fct trm')
    where
        Right (ts', mulp) = mulop ts
        Right (ts'', fct) = factor ts'
        Right (ts''', trm') = term' ts''
-}
term' :: [Token] -> Either [Char] ([Token], Term')
term' ts = case mulop ts of
    Right (ts', mulp) -> case factor ts' of
        Right (ts'', fct) -> case term' ts'' of
            Right (ts''', trm') -> Right (ts''', R23 mulp fct trm')
            Left msg -> Left msg
        Left msg -> Left msg
    Left msg -> Left msg







{-
prog :: [Token] -> Maybe ([Token], Prog)
prog ts =  (\(ts', pt) -> (ts', (R1 pt))) $ stmt ts

stmt :: [Token] -> Maybe ([Token], Stmt)
stmt (t:ts) = case t of
--    (TIF p)    ->  
--    (TWHILE p) -> 
    TINPUT p  ->  (\(t':ts') -> case t' of
                        TID var p   -> (ts', R4 PINPUT $ PID var)
                        otherwise   ->  error "Invalid Pattern R4") ts
    TID var p ->  (\(ts', pt) -> (ts', R5 (PID var) PASSIGN pt)) 
                  expr $ (\(t':ts'') -> case t' of
                        TASSIGN p   -> ts''
                        otherwise   -> error "Invalid Pattern R5") ts
--    (TWRITE p) -> 
--    BEGIN ->

addop :: [Token] -> ([Token], Addop)
addop (t:ts) = case t of
        TADD p  -> (ts, R8 PADD)
        TSUB p  -> (ts, R9 PSUB)
        _       -> error "Invalid Pattern Addop"

mulop :: [Token] -> ([Token], Mulop)
mulop (t:ts) = case t of
        TMUL p  -> (ts, R10 PMUL)
        TDIV p  -> (ts, R11 PDIV)
        _       -> error "Invalid Pattern Mulop"

stmtlist :: [Token] -> ([Token], Stmtlist)
stmtlist ts = (\(ts', pt) -> (ts', R16 pt)) $ stmtlist' ts

stmtlist' :: [Token] -> ([Token], Stmtlist')
stmtlist' ts = (\((t':ts'),pt) -> case t' of
            TSEMICOLON -> (\(ts'', pt') -> (ts'', R17 pt PSC pt' )) $ stmtlist' ts'
            
 ) $ stmt ts

expr :: [Token] -> ([Token], Expr)
expr (t:ts) = (

term :: [Token] -> ([Token], Term)
term ts = (\(ts', pt) ->  (\(ts'',pt') -> (ts'', R22 pt pt')) term' ts') $ factor ts

term' :: [Token] -> ([Token], Term')
term' ts = (ts, R24)
-}
main = do
    file <- getArgs
    s <-readFile (file !! 0)
    let toks = lexer s
    case toks of
        Right ts -> print $ factor ts
        Left str -> error str



{-
main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        0 -> do
            isFile <- doesFileExist (args !! -1)
            case isFile of
                True -> do
                    content <- readFile (args !! -1)
                    pPrint $ lexer content
                False -> error "Error! File not found."
        1 -> case (args !! 0) of
                "-l" -> do
                    isFile <- doesFileExist (args !! 0)
                    case isFile of
                        True -> do
                            content <- readFile (args !! 0)
                            print $ lexer content
                        False -> error "Error! File not found."
        otherwise -> error "Error! Invalid use. expected either the argument(s) \"./Lexer <filename>\" for pretty printed output, or \"./Lexer -l <filename>\" for the token list as output. "
 -}

test0 = lexer "input x"
