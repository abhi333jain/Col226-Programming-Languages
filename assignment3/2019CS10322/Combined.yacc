

%%
(* required declarations *)

%name Check

%term
      EOF| TERM of string| CONST of bool| ID of string | NOT of string| AND of string| NUM of int|
      OR of string| XOR of string| EQUALS of string| IMPLIES of string | IF of string| 
      THEN of string| ELSE of string| FI of string| LPAREN of string| RPAREN of string| 
      PLUS of string |MINUS of string|TIMES of string|NEGATE of string|LESSTHAN of string|
      GREATERTHAN of string | LET of string | IN of string| END of string | EQ of string |
      FN of string | COLON of string | IMP of string| FUN of string | ARROW of string |
      INT of string| BOOL of string


%nonterm 
      start of AST.program | program of AST.program  | statement of AST.statement | 
      exp of AST.exp  | decl of AST.decl | fntype of AST.typ |fundef of AST.decl

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right ARROW
%right IMP
%right IF 
%right THEN
%right ELSE
%left  LESSTHAN GREATERTHAN
%right IMPLIES
%left AND OR XOR PLUS MINUS
%left TIMES
%right NOT NEGATE

%nonassoc EQUALS 
%start start

%verbose

%%


start: program (program)
      |  (AST.EMPTY)

program: statement (AST.Statement(statement))
        | statement program(AST.Program(statement,program))

statement: exp (AST.Expression(exp))
          | exp TERM (AST.Expression(exp))
          | fundef (AST.Declaration(fundef))
          | fundef TERM (AST.Declaration(fundef))

decl: ID EQ exp (AST.ValDecl(ID, exp)) 
      | fundef (fundef)

fntype : INT (AST.INT)
        | BOOL (AST.BOOL) 
        | LPAREN fntype RPAREN (fntype)
        | fntype ARROW fntype (AST.ARROW(fntype1,fntype2))

fundef : FUN ID LPAREN ID COLON fntype RPAREN COLON fntype IMP exp (AST.FunDecl((ID1),AST.Fn(ID2,fntype1,fntype2,exp)))

exp :    CONST (AST.ConstExp(CONST))
        
        | NUM (AST.NumExp(NUM))

        | ID (AST.VarExp(ID))

        | NOT exp (AST.UnaryExp(AST.NOT,exp))

        | NEGATE exp (AST.UnaryExp(AST.NEGATE,exp))

        | LPAREN exp exp RPAREN  (AST.AppExp(exp1,exp2))

        | LPAREN exp RPAREN  (exp)

        | exp PLUS exp (AST.BinExp(AST.ADD, exp1,  exp2))

        | exp MINUS exp (AST.BinExp(AST.MINUS, exp1,  exp2))

        | exp TIMES exp (AST.BinExp(AST.TIMES, exp1,  exp2))

        | exp LESSTHAN exp (AST.BinExp(AST.LESSTHAN, exp1,  exp2))
        
        | exp GREATERTHAN exp (AST.BinExp(AST.GREATERTHAN, exp1,  exp2))

        | exp AND exp (AST.BinExp(AST.AND, exp1,  exp2))

        | exp OR exp (AST.BinExp(AST.OR, exp1,  exp2))

        | exp XOR exp (AST.BinExp(AST.XOR, exp1,  exp2))

        | exp EQUALS exp (AST.BinExp(AST.EQUALS, exp1,  exp2))

        | exp IMPLIES exp (AST.BinExp(AST.IMPLIES, exp1,  exp2))

        | IF exp THEN exp ELSE exp FI (AST.Ite(exp1, exp2, exp3) )

        | LET decl IN exp END (AST.LetExp(decl, exp))

        | FN LPAREN ID COLON fntype RPAREN COLON fntype IMP exp (AST.Fn(ID,fntype1,fntype2,exp))

  




(*

      | ID LPAREN exp RPAREN (AST.AppExp(AST.VarExp(ID),exp))

start: program ( program^(", start -> program "))
      |  ("start -> epsilon" )

program: statement ( statement^(", program -> statement" ))
        | program statement (program^", "^statement^(", program -> program statement"))

statement: exp TERM ( exp^", TERM ;"^(", statement -> exp TERM"))

exp : CONST ( "CONST "^CONST^(", exp -> CONST" ) )

        | ID ( "ID "^ID^(", exp -> ID" ))

        | NOT exp ( "NOT NOT, " ^ exp ^ (", exp -> NOT exp" ))

        | LPAREN exp RPAREN ( "LPAREN (, " ^ exp ^ ", RPAREN ), " ^ (", exp -> LPAREN exp RPAREN" ) )

        | exp PLUS exp ( exp1 ^ ",AND AND, " ^ exp2 ^ (", exp -> exp AND exp" ))
        | exp MINUS exp ( exp1 ^ ",MINUS MINUS, " ^ exp2 ^ (", exp -> exp AND exp" ))
        | exp TIMES exp ( exp1 ^ ",TIMES TIMES, " ^ exp2 ^ (", exp -> exp TIMES exp" ))
        | exp LESSTHAN exp ( exp1 ^ ",LESSTHAN LESSTHAN, " ^ exp2 ^ (", exp -> exp LESSTHAN exp" ))

        | exp GREATERTHAN exp ( exp1 ^ ",GREATERTHAN GREATERTHAN, " ^ exp2 ^ (", exp -> exp GREATERTHAN exp" ))

        | exp AND exp ( exp1 ^ ",AND AND, " ^ exp2 ^ (", exp -> exp AND exp" ))

        | exp OR exp ( exp1 ^ ", OR OR, " ^ exp2 ^ (", exp -> exp OR exp" ))

        | exp XOR exp ( exp1 ^ ", XOR XOR, " ^ exp2 ^ (", exp -> exp XOR exp" ))

        | exp EQUALS exp ( exp1 ^ ", EQUALS EQUALS, " ^ exp2 ^ (", exp -> exp EQUALS exp" ))

        | exp IMPLIES exp ( exp1 ^ ", IMPLIES IMPLIES, " ^ exp2 ^ (", exp -> exp IMPLIES exp" ))

        | IF exp THEN exp ELSE exp ( "IF IF, " ^ exp1 ^ ", THEN THEN, " ^exp2 ^ ", ELSE ELSE, " ^exp3 ^ 
        (", exp -> IF exp THEN exp ELSE exp" ) )


*)





(*
start: program ( print(" start -> program \n"))
      |  (print("start -> epsilon \n" ))

program: statement ( print(" program -> statement \n" ))
        | program statement (print(", program -> program statement statement \n"))

statement: exp TERM ( print(", statement -> exp TERM \n"))

exp : CONST ( print(", exp -> " ^ CONST ^ "\n") )

        | ID ( print(", exp -> " ^ ID ^ "\n"))

        | NOT exp ( print("exp -> NOT exp \n" ))

        | LPAREN exp RPAREN ( print(" exp -> LPAREN exp RPAREN \n" ) )

        | exp AND exp ( print("exp -> exp AND exp \n" ))

        | exp OR exp ( print(", exp -> exp OR exp \n" ))

        | exp XOR exp ( print (", exp -> exp XOR exp \n" ))

        | exp EQUALS exp ( print(", exp -> exp EQUALS exp \n" ))

        | exp IMPLIES exp ( print(", exp -> exp IMPLIES exp \n" ))

        | IF exp THEN exp ELSE exp ( print (", exp -> IF exp THEN exp ELSE exp \n" ) )







*)











