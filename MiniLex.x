-----------------------------------------------------------------------------
-- Haskell code to initialize the module
-----------------------------------------------------------------------------
{
module MiniLex where

import System.Environment
import System.Directory
import Data.Char
}

-----------------------------------------------------------------------------
-- Alex macro and wrapper definitions
-----------------------------------------------------------------------------
%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$newline = [\n]

@comment = \%.* $newline+
@var = $alpha [$alpha $digit \_ \']*
@num = $digit+
@assign = ":="
@add = \+
@sub = \-
@mul = \*
@div = \/
@lpar = "("
@rpar = ")"
@semi = \;

-----------------------------------------------------------------------------
-- Lexer pattern matching definitions
-----------------------------------------------------------------------------
tokens :-
    @comment    ;
    "/*"    {multi}
    $white+     ;
    if      {\(p,_,_,_) i -> return $ TUP (TIF, p)}
    then    {\(p,_,_,_) i -> return $ TUP (TTHEN, p)}
    while   {\(p,_,_,_) i -> return $ TUP (TWHILE, p)}
    do      {\(p,_,_,_) i -> return $ TUP (TDO, p)}
    input   {\(p,_,_,_) i -> return $ TUP (TINPUT, p)}
    else    {\(p,_,_,_) i -> return $ TUP (TELSE, p)}
    begin   {\(p,_,_,_) i -> return $ TUP (TBEGIN, p)}
    end     {\(p,_,_,_) i -> return $ TUP (TEND, p)}
    write   {\(p,_,_,_) i -> return $ TUP (TWRITE, p)}
    @assign {\(p,_,_,_) i -> return $ TUP (TASSIGN, p)}
    @add    {\(p,_,_,_) i -> return $ TUP (TADD, p)}
    @sub    {\(p,_,_,_) i -> return $ TUP (TSUB, p)}
    @mul    {\(p,_,_,_) i -> return $ TUP (TMUL, p)}
    @div    {\(p,_,_,_) i -> return $ TUP (TDIV, p)}
    @lpar   {\(p,_,_,_) i -> return $ TUP (TLPAR, p)}
    @rpar   {\(p,_,_,_) i -> return $ TUP (TRPAR, p)}
    @semi   {\(p,_,_,_) i -> return $ TUP (TSEMICOLON, p)}
    @num    {\(p,_,_,s) i -> return $ TUP (TNUM (read (take i s)), p)}
    @var    {\(p,_,_,s) i -> return $ TUP (TID (take i s), p)}
    "*/"    {\alexIn i -> myErr alexIn "Unbalanced Multiline"}
    .{1}    {unknown}

{
-----------------------------------------------------------------------------
-- Taken from Haskell.Language.TH source code then slightly modified
-----------------------------------------------------------------------------
byteToString :: Byte -> Char
byteToString = chr . fromIntegral


-----------------------------------------------------------------------------
-- Generate an error when an unknown character is found in the input string
-----------------------------------------------------------------------------
unknown :: AlexInput -> Int -> Alex TokenPos
unknown (p, c, bs, s) n = myErr (p, c, bs, s) ("Unknown Character: \"" ++ (take 1 s) ++ "\"")

-----------------------------------------------------------------------------
-- Give the error message meaningful output (position of error from input)
-----------------------------------------------------------------------------
myErr :: AlexInput -> String -> Alex TokenPos
myErr ((AlexPn p1 p2 p3), c, bs, s) mssg = error $ mssg ++ " at line " ++ (show p2) ++ ", column " ++ (show p3)

-----------------------------------------------------------------------------
-- A wrapper function to track how many opening multi-line comments need closing
-----------------------------------------------------------------------------
multi :: AlexInput -> Int -> Alex TokenPos
multi alexIn i = do
        currentIn <- alexGetInput
        alexSetInput $ multi' currentIn 1
        alexMonadScan

-----------------------------------------------------------------------------
-- Helper for multi, so the counter can be initialized to 0 at first call
-- The logic is to case over the input string byte-by-byte, discarding anything
-- found that isn't a closing delimeter. However; single line comments take precedent
-- (thus, ignore the entire line following any '%', including closing delimeters).
-----------------------------------------------------------------------------        
multi' :: (Eq a, Num a) => AlexInput -> a -> (AlexPosn, Char, [Byte], String)
multi' alexIn 0 = alexIn
multi' alexIn n = do
        case (alexGetByte alexIn) of
            Nothing -> error "Unbalanced Multi-Line Comment 1"
            Just (nextChar, rest) -> do
                case (byteToString nextChar) of
                    '*' -> do
                        case (alexGetByte rest) of
                            Nothing -> error "Unbalanced Multi-Line Comment 2"
                            Just (nextChar', rest') -> do
                                case (byteToString nextChar') of
                                    '/' -> multi' rest' (n - 1)
                                    _   -> multi' rest' n
                    '/' -> do
                        case (alexGetByte rest) of
                            Nothing -> error "Unbalanced Multi-Line Comment 3"
                            Just (nextChar', rest') -> do
                                case (byteToString nextChar') of
                                    '*' -> multi' rest' (n + 1)
                                    _   -> multi' rest' n
                    '%' -> do
                        case (endLine rest) of
                            Nothing -> error "Unbalanced Multi-Line Comment 4"
                            Just (rest') -> multi' rest' n

                    _   -> multi' rest n


-----------------------------------------------------------------------------
-- This is a helper function for multi' in the case a single line comment is
-- found. This will recursively analyze the string byte-by-byte, only stopping
-- when a new-line is encountered.
-----------------------------------------------------------------------------
endLine :: AlexInput -> Maybe AlexInput
endLine alexIn = do
        case alexGetByte alexIn of
            Nothing -> Nothing
            Just (nextChar, rest) -> do
                case (byteToString nextChar) of
                    '\n'-> Just (rest)
                    _   -> endLine rest


-----------------------------------------------------------------------------
-- This function reads the next token from the input string, and recursively
-- does so until the EOF.
-----------------------------------------------------------------------------
tokens :: [TokenPos] -> Alex [TokenPos]
tokens ts = do
        next <- alexMonadScan
        case next of
            TEOF -> return ts
            otherwise -> tokens (ts ++ [next])

-----------------------------------------------------------------------------
-- The data type for lexical Tokens.
-----------------------------------------------------------------------------

data TokenPos = TUP (Token, AlexPosn)
                |TEOF

data Token = TIF 
            |TTHEN
            |TWHILE
            |TDO 
            |TINPUT 
            |TELSE 
            |TBEGIN 
            |TEND 
            |TWRITE 
            |TID [Char] 
            |TNUM Int 
            |TADD 
            |TASSIGN
            |TSUB 
            |TMUL 
            |TDIV 
            |TLPAR 
            |TRPAR 
            |TSEMICOLON 
    deriving (Show, Eq)

-----------------------------------------------------------------------------
-- When showing the user the data, the position of the pattern is irrelevant,
-- defining our own instance of Show allows us to maintain the information
-- as it needs to be passed on to the parser eventually, without creating a 
-- mess for debugging.
-----------------------------------------------------------------------------
instance Show TokenPos where
    show tok = case tok of
        TUP (t, p) -> show t

instance Eq TokenPos where
    (==) (TUP (t1, _)) (TUP (t2, _)) = (t1 == t2)

-----------------------------------------------------------------------------
-- I honestly have no idea why Alex required us to define this.
-----------------------------------------------------------------------------
alexEOF :: Alex TokenPos
alexEOF = return TEOF

-----------------------------------------------------------------------------
-- The main routine of this module (when not served as a stand-alone application)
-----------------------------------------------------------------------------
lexer :: String -> [TokenPos]
lexer inStr = do
        case (runAlex inStr $ tokens []) of
            Right ts -> ts
            Left mssg -> error "Lex Fail"

-----------------------------------------------------------------------------
-- Easier to read than printing out the list in-line. This prints each token
-- on it's own individual line. (I believe, mapM_ is required due to Alex's 
-- monad wrapper)
-----------------------------------------------------------------------------
pPrint :: [TokenPos] -> IO ()
pPrint tokens = mapM_  (putStrLn.show) tokens


}
