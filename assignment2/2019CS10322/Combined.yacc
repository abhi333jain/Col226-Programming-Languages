(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

(*
fun detach (x) = 
  if(x=)

*)

%%
(* required declarations *)

%name Check

%term
      EOF| TERM of string| CONST of string| ID of string | NOT of string| AND of string| 
      OR of string| XOR of string| EQUALS of string| IMPLIES of string | IF of string| 
      THEN of string| ELSE of string| LPAREN of string| RPAREN of string  


%nonterm 
      start of string| program of string | statement of string| formula of string 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right IF
%right THEN
%right ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
  (* %nonassoc*)
%start start

%verbose

%%

start: program ( program^(", start -> program "))
      |  ("[ start -> epsilon ]" )

program: statement ( statement^(", program -> statement" ))
        | program statement (program^", "^statement^(", program -> statement"))

statement: formula TERM ( formula^", TERM ;"^(", statement -> formula TERM"))

formula : CONST ( "CONST "^CONST^(", formula -> CONST" ) )

        | ID ( "ID "^ID^(", formula -> ID" ))

        | NOT formula ( "NOT NOT, " ^ formula ^ (", formula -> NOT formula" ))

        | LPAREN formula RPAREN ( "LPAREN (, " ^ formula ^ ", RPAREN ), " ^ (", formula -> LPAREN formula RPAREN" ) )

        | formula AND formula ( formula1 ^ ",AND AND, " ^ formula2 ^ (", formula -> formula AND formula" ))

        | formula OR formula ( formula1 ^ ", OR OR, " ^ formula2 ^ (", formula -> formula OR formula" ))

        | formula XOR formula ( formula1 ^ ", XOR XOR, " ^ formula2 ^ (", formula -> formula XOR formula" ))

        | formula EQUALS formula ( formula1 ^ ", EQUALS EQUALS, " ^ formula2 ^ (", formula -> formula EQUALS formula" ))

        | formula IMPLIES formula ( formula1 ^ ", IMPLIES IMPLIES, " ^ formula2 ^ (", formula -> formula IMPLIES formula" ))

        | IF formula THEN formula ELSE formula ( "IF IF, " ^ formula1 ^ ", THEN THEN, " ^formula2 ^ ", ELSE ELSE, " ^formula3 ^ 
        (", formula -> IF formula THEN formula ELSE formula" ) )



(*
start: program ( print(" start -> program \n"))
      |  (print("start -> epsilon \n" ))

program: statement ( print(" program -> statement \n" ))
        | program statement (print(", program -> program statement statement \n"))

statement: formula TERM ( print(", statement -> formula TERM \n"))

formula : CONST ( print(", formula -> " ^ CONST ^ "\n") )

        | ID ( print(", formula -> " ^ ID ^ "\n"))

        | NOT formula ( print("formula -> NOT formula \n" ))

        | LPAREN formula RPAREN ( print(" formula -> LPAREN formula RPAREN \n" ) )

        | formula AND formula ( print("formula -> formula AND formula \n" ))

        | formula OR formula ( print(", formula -> formula OR formula \n" ))

        | formula XOR formula ( print (", formula -> formula XOR formula \n" ))

        | formula EQUALS formula ( print(", formula -> formula EQUALS formula \n" ))

        | formula IMPLIES formula ( print(", formula -> formula IMPLIES formula \n" ))

        | IF formula THEN formula ELSE formula ( print (", formula -> IF formula THEN formula ELSE formula \n" ) )







*)











