# MiniParser
A recursive descent parser for the Minisculus language using my past project, MiniLex (utilizing Haskell's Alex).

### Compile
This is written for Haskell v8+

> ghc Parser.hs

### Execution
To run individual parsing on a file run:

> $./Parser \<test-file\>

#### Context Free Grammar
The original grammar provided by Dr. Cockett for the Minisculus language is neither a LL(1) grammar, nor recursive descent.


Non-Terminal| Production
---         | --- 
 prog ->    | stmt.
 stmt ->    | IF expr THEN stmt ELSE stmt
` `         | \| WHILE expr DO stmt
` `         | \| INPUT ID
` `         | \| ID ASSIGN expr
` `         | \| WRITE expr
` `         | \| BEGIN stmtlist END.
stmtlist -> | stmtlist stmt SEMICOLON
` `         | \|.
 expr ->    | expr addop term
` `         | \| term.
 term ->    | term mulop factor 
` `         | \| factor. 
 factor ->  | LPAR expr RPAR 
` `         | \| ID 
` `         | \| NUM 
` `         | \| SUB NUM. 
 addop ->   | ADD 
` `         | \| SUB. 
 mulop ->   | MUL 
` `         | \| DIV. 



With the following modifications, we can produce an LL(1) grammar:

Non-Terminal | Production
--- | ---
 stmtlist -> | stmtlist'.
 stmtlist' -> | stmt SEMICOLON stmtlist'
 ` ` | \|.
 expr -> | term expr'.
 expr' -> | addop term expr'
` ` |        \|.
 term -> | factor term'.
 term' -> | mulop factor term'
` ` |        \|.


However, this is still not in recursive descent form!
Again, with the modifications we can change this:

Non-Terminal    | Production                |` `| Non-Terminal  | Production
---             | ---                       | --- | ---         | ---
stmt ->         | IF expr thenpart elsepart |` `|stmtlist ->    | stmt semipart
thenpart ->     | THEN stmt.                |` `|semipart ->    | SEMICOLON stmtlist.
elsepart ->     | ELSE stmt.                |` `|` `            | ` `
` `             | ` `                       |` `|expr' ->       | ADD term exp'
stmt ->         | WHILE expr dopart         |` `|` `            | \|SUB term expr'
dopart ->       | DO stmt.                  |` `|` `            | \|.
` `             | ` `                       |` `|` `            | ` `
stmt ->         | BEGIN stmtlist endpart.   |` `|term' ->       | MUL factor term'
endpart ->      | END.                      |` `|` `            | \|DIV factor term'
` `             | ` `                       |` `|` `            | \|.

Then our final grammar looks like:

Non-Terminal    | Production                |` `| Non-Terminal    | Production
---             | ---                     | --- |    ---        | ---
stmt ->         | IF expr thenpart elsepart |` `| thenpart ->     | THEN stmt.
` `             | \|WHILE expr dopart      |` `| elsepart ->     | ELSE stmt.
` `             | \|INPUT ID                |` `| dopart ->       | DO stmt.
` `             | \|ID ASSIGN expr          |` `| endpart ->      | END.
` `             | \|WRITE expr              |` `|` `              |` `
` `             | \|BEGIN stmtlist endpart. |` `|` `              |` `
stmtlist ->     | stmt semipart             |` `| semipart ->     | SEMICOLON stmtlist.
` `             | \|.                       |` `| expr' ->        | ADD term expr'
expr ->         | term expr'.               |` `|` `              | \|SUB term expr'
term ->         | factor term'.             |` `|` `              | \|.
term' ->        | MUL factor term'          |` `| factor ->       | LPAR expr clopar
` `             | \|DIV factor term'        |` `|` `              | \|ID
` `             | \|.                       |` `|` `              | \|NUM
clopar ->       | RPAR.                     |` `|` `              | \|SUB NUM.


