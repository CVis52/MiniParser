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
stmt -> IF expr thenpart                R1
      | WHILE expr dopart               R2
      | INPUT ID                        R3
      | ID ASSIGN expr                  R4
      | WRITE expr                      R5
      | BEGIN stmtlist endpart.         R6
thenpart -> THEN stmt elsepart.         R7
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
        deriving (Show)

data Exp = Add Exp Exp
            |Mul Exp Exp
            |Div Exp Exp
            |Neg Exp
            |Id String
            |Num Integer
        deriving (Show)

stmt :: [TokenPos] -> Either String ([TokenPos], Stmt)
stmt ((TUP(t, p)):ts) = case t of
    TWHILE -> case expr ts of
        Right (r, e) -> dopart e r
        Left msg -> Left msg
    TINPUT -> case ts of
        (TUP(TID str, p):r) -> Right (r, Input $ Id str)
        _ -> Left $ "Error: Fail in stmt, rule R3.\nThe token INPUT at " ++ (pretPn p) ++ " is not followed by an ID token." 
    TID v -> case ts of
        (TUP(TASSIGN, p):r) -> case expr r of
            Right (r', e) -> Right (r', Assign v e)
            Left msg -> Left msg
        (TUP(t, p):r) -> Left $ "Error:  Fail in stmt, rule R4.\nThe token ID at " ++ (pretPn p) ++ " is not followed by an ASSIGN token.\nActual: " ++ (show t) ++ "."
        _ -> Left $ "Error: Fail in stmt, rule R4.\nThe token ID at " ++ (pretPn p) ++ " is not followed by an ASSIGN token, assuming token list is empty."
    TWRITE -> case expr ts of
        Right (r, e) -> Right (r, Print e)
        Left msg -> Left msg
    TBEGIN -> case stmtlist ts of
        Right (r, ss) -> case endpart r of
            Just r' -> Right (r', Block ss)
            Nothing -> Left $ "Error: Fail in stmt, rule R6.\nThere was no END token for the BEGIN block started at " ++ (pretPn p)  ++ "."
        Left msg -> Left msg 
    TIF -> case expr ts of
        Right (r, e) -> thenpart e r
        Left msg -> Left msg
    _ -> Left $ "Error: Fail in stmt.\n Found the token: " ++ (show t) ++ " at " ++ (pretPn p) ++ ".\nNon-exhaustive patterns." -- Error
stmt oth = Left "Error: Empty token list passed to stmt."

thenpart :: Exp -> [TokenPos] -> Either String ([TokenPos], Stmt)
thenpart e ((TUP(TTHEN, p)):ts) = case stmt ts of
    Right (r, s) -> elsepart e s r
    Left msg -> Left msg
thenpart _ oth = Left "Error: THEN token not found in thenpart."

elsepart :: Exp -> Stmt -> [TokenPos] -> Either String ([TokenPos], Stmt)
elsepart e s ((TUP(TELSE, p)):ts) = case stmt ts of
    Right (r, s') -> Right (r, If e s s')
    Left msg -> Left msg
elsepart _ _ oth = Left "Error: ELSE token not found in elsepart."

dopart :: Exp -> [TokenPos] -> Either String ([TokenPos], Stmt)
dopart e ((TUP(TDO, _)):ts) = case stmt ts of
    Right (r, s) -> Right (r, While e s)
    Left msg -> Left msg
dopart _ oth = Left "Error: DO token not found in dopart."

endpart :: [TokenPos] -> Maybe [TokenPos]
endpart ts = case ts of
    ((TUP(TEND, p)):r) -> Just r
    _ -> Nothing 


stmtlist :: [TokenPos] -> Either String ([TokenPos], [Stmt])
stmtlist ts = case stmt ts of
    Right (r, s) -> case semipart r of
        Right (r', ss) -> Right (r', ([s] ++ ss))
        Left msg -> Left msg
    Left msg -> Right (ts, [])

semipart :: [TokenPos] -> Either String ([TokenPos], [Stmt])
semipart ((TUP(TSEMICOLON, p)):ts) = stmtlist ts
semipart ((TUP(t, p)):ts) = Left $ "Error: SEMICOLON token not found.\nActual: " ++ (show t) ++ " at " ++ (pretPn p) ++ "."
semipart [] = Left "Error: Empty token list sent to semipart."

expr :: [TokenPos] -> Either String ([TokenPos], Exp)
expr ts = case term ts of
    Right (r, e) -> (\(r', f) -> Right (r', f e)) $ expr' r
    Left msg -> Left msg

expr' :: [TokenPos] -> ([TokenPos], (Exp -> Exp))
expr' (t:ts) = case t of
    TUP(TADD,p) -> case term ts of
        Right (r, e) -> (\(r', f) -> (r', (\x -> f (Add x e)))) $ expr' r
        Left msg -> (ts, (\x -> x))
    TUP(TSUB,p) -> case term ts of
        Right (r, e) -> (\(r', f) -> (r', (\x -> f (Add x (Neg e))))) $ expr' r
        Left msg -> (ts, (\x -> x))
    _ -> ((t:ts), (\x -> x))

term :: [TokenPos] -> Either String ([TokenPos], Exp)
term ts = case factor ts of
    Right (r, e) -> (\(r', f) -> Right (r', f e)) $ term' r
    Left msg -> Left msg

term' :: [TokenPos] -> ([TokenPos], (Exp -> Exp))
term' (t:ts) = case t of
    TUP(TMUL,p) -> case factor ts of
        Right (r, e) -> (\(r', f) -> (r', (\x -> f (Mul x e)))) $ term' r
        Left msg -> ((t:ts), (\x -> x))
    TUP(TDIV,p) -> case factor ts of
        Right (r, e) -> (\(r', f) -> (r', (\x -> f (Div x e)))) $ term' r
        Left msg -> ((t:ts), (\x -> x))
    _ -> ((t:ts), (\x -> x))

factor :: [TokenPos] -> Either String ([TokenPos], Exp)
factor (t:ts) = case t of
    TUP(TLPAR, p) -> case expr ts of
        Right (r, e) -> case clopar r of
            Just r' -> Right (r', e)
            Nothing -> Left "No closing bracket for an expression."
        Left msg -> Left msg
    TUP(TID v, p) -> Right (ts, Id v)
    TUP(TNUM n, p) -> Right (ts, Num (toInteger n))
    TUP(TSUB, p) -> case ts of
        (TUP(TNUM n,p):r) -> Right (r, Neg (Num (toInteger n)))
        otherwise -> Left "Error: Sub wasn't followed by Num in factor."
    otherwise -> Left "Error: invalid pattern passed to factor."
factor ts = Left "Error: Empty list passed to factor."


clopar :: [TokenPos] -> Maybe [TokenPos]
clopar ((TUP(TRPAR, _)):ts) = Just ts
clopar ts = Nothing

pretPn :: AlexPosn -> String
pretPn (AlexPn d l c) = "Line " ++ (show l) ++ ", column " ++ (show c)


-----------------------------------------------------------------------------
-- Stack code generation
----------------------------------------------------------------------------- 
-- cPUSH k --- push constant k onto stack 
--
-- rPUSH r --- push contents of register r onto stack 
--
-- sPUSH --- replaces the top element of the stack by the element it indexes 
-- in the stack
--
-- LOAD r --- pop the top of the stack and put the value in register r 
--
-- OPn?? --- perform the operation on the top n values of the stack replacing 
-- them by the result
--
-- cJUMP L --- conditional goto L (a label) pops top of stack and if it is 
-- zero (false) it jumps to label
-- 
-- JUMP L --- unconditional jump to label 
-- 
-- PRINT --- pops and prints the top element of the stack 
-- 
-- READ r --- reads a value into register r (actually it reads a line and 
-- uses the first value on the line ...)
-----------------------------------------------------------------------------

stmtToStack :: Int -> Stmt -> (Int, String)
stmtToStack n statement = case statement of
    If e s s' -> (\(n', str) -> 
                    (\(n'', str') ->  
                        (n'', (expToStack e) ++ "cJUMP L" ++ (show n) ++ "\n" 
                            ++ str ++ "JUMP L" ++ (show (n + 1)) ++ "\n" 
                            ++ "L" ++ (show n) ++ ":\n" 
                            ++ str' ++ "L" ++ (show (n + 1)) ++ ":\n")
                    ) $ stmtToStack n' s'
                ) $ stmtToStack (n + 2) s

    While e s -> (\(n', str) -> 
                    (n', "L" ++ (show n) ++ ":\n" 
                        ++ (expToStack e) ++ "cJUMP L" ++ (show $ n + 1) ++ "\n" 
                        ++ str ++ "JUMP L" ++ (show n) ++ "\n" 
                        ++ "L" ++ (show $ n+1) ++ ":\n")
                ) $ stmtToStack (n + 1) s

    Assign v e -> (n, (expToStack e) ++ "LOAD " ++ v ++ "\n")

    Block [] -> (n, "")
    Block (s:ss) -> (\(n', str) -> 
                        (\(n'', str') ->  
                            (n'', str ++ str')
                        ) $ stmtToStack n' (Block ss)
                    ) $ stmtToStack (n + 1) s

    Print e -> (n, (expToStack e) ++ "PRINT\n")

    Input (Id v) -> (n, "READ " ++ v ++ "\n")
    

expToStack :: Exp -> String
expToStack e = case e of
    Add e1 e2 -> (expToStack e1) ++ (expToStack e2) ++ "OP2 +" ++ "\n"
    Mul e1 e2 -> (expToStack e1) ++ (expToStack e2) ++ "OP2 *" ++ "\n"
    Div e1 e2 -> (expToStack e1) ++ (expToStack e2) ++ "OP2 /" ++ "\n"
    Neg e1 -> (expToStack e1) ++ "OP1 -" ++ "\n"
    Id v -> "rPUSH " ++ v ++ "\n"
    Num n -> "cPUSH " ++ (show n) ++ "\n"



main = do
    args <- getArgs
    let file = (args !! 0)
    s <-readFile file
    let toks = lexer s
    let ast = stmt toks
    case ast of
        Right ([], smt) -> do
            putStrLn $ "Success\nThe syntax tree: \n" ++ (show ast) ++ "\n\nThe output file: default.txt"
            writeFile "default.txt" ((\(_, code) -> code) $ stmtToStack 0 smt)
        Right ((t:ts), smt) -> print "Invalid parse of, " ++ (show file) ++ ", non-empty token list."
        Left msg -> putStrLn $ "Invalid parse of, " ++ (show file) ++ "\n" ++ msg

