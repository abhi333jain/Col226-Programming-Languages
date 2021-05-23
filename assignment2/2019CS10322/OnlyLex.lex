datatype lexresult= EOF| TERM of string| CONST of string| ID of string | NOT of string| AND of string| 
                    OR of string| XOR of string| EQUALS of string| IMPLIES of string | IF of string| 
                    THEN of string| ELSE of string| LPAREN of string| RPAREN of string

val str = ref "["
val iserror = ref 0
val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;
val eof = fn () => ( if(!iserror=0) then print( !str^"\b"^"\b"^ "]"^"\n") else OS.Process.exit(OS.Process.success); EOF)

val error = fn (x,lin,col) => TextIO.output(TextIO.stdOut,
							("Unknown Token:" ^ (Int.toString(lin))^":"^(Int.toString(col))^": "^x^"\n"));

%%

%structure OnlyLex
alpha=[A-Za-z];
ws = [\ \t];

%%

\n       => (lin:=(!lin)+1;  col:=0 ;lex() );
{ws}+    => (col:= !col + size yytext; lex());
";"      => (col:= !col + size yytext; str := !str^"TERM \""^yytext^"\", ";  TERM(";"));
"("      => (col:= !col + size yytext; str := !str^"LPAREN \""^yytext^"\", "; LPAREN("("));
")"      => (col:= !col + size yytext; str := !str^"RPAREN \""^yytext^"\", ";  RPAREN(")"));
{alpha}+ => (col:= !col + size yytext;  
                if(yytext="AND") then ( str := !str^"AND \""^yytext^"\", "; AND("AND") )            
                else if(yytext="OR") then ( str := !str^"OR \""^yytext^"\", "; OR("OR") )
                else if(yytext="XOR") then ( str := !str^"XOR \""^yytext^"\", "; XOR("XOR") )
                else if(yytext="NOT") then ( str := !str^"NOT \""^yytext^"\", "; NOT("NOT") )
                else if(yytext="EQUALS") then ( str := !str^"EQUALS \""^yytext^"\", "; EQUALS("EQUALS") )
                else if(yytext="IMPLIES") then ( str := !str^"IMPLIES \""^yytext^"\", "; IMPLIES("IMPLIES") )
                else if(yytext="TRUE") then ( str := !str^"CONST \""^yytext^"\", "; CONST("TRUE") )
                else if(yytext="FALSE") then ( str := !str^"CONST \""^yytext^"\", "; CONST("FALSE") )
                else if(yytext="IF") then ( str := !str^"IF \""^yytext^"\", ";    IF("IF") )
                else if(yytext="ELSE") then ( str := !str^"ELSE \""^yytext^"\", ";  ELSE("ELSE") )
                else if(yytext="THEN") then ( str := !str^"THEN \""^yytext^"\", ";  THEN("THEN") )
                else (str := !str^"ID \""^yytext^"\", ";  ID yytext  )
                );
.        => (col:= !col + size yytext; ( iserror :=1 ; error(yytext,!lin,!col)); lex());         