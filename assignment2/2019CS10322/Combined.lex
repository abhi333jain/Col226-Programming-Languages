structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

val iserror = ref 0
val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;
val ls = ref [""]
val eof = fn () => (Tokens.EOF(!lin, !col))
val tokens_disp = fn () => !ls

val error = fn (x,lin,col) => TextIO.output(TextIO.stdOut,
							("Unknown Token:" ^ (Int.toString(lin))^":"^(Int.toString(col))^": "^x^"\n"));



  
%%
%header (functor CheckLexFun(structure Tokens:Check_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];

%%

\n       => (lin:=(!lin)+1; col:=0 ;lex() );
{ws}+    => (col:= !col + size yytext; lex());
"AND"    => (col:= !col + size yytext;  ls := "AND":: !ls ;   Tokens.AND("AND",!lin,!col) );
"OR"     => (col:= !col + size yytext;  ls := "OR"::  !ls   ;  Tokens.OR("OR",!lin,!col));
"XOR"    => (col:= !col + size yytext;  ls := "XOR":: !ls   ; Tokens.XOR("XOR",!lin,!col));
"NOT"    => (col:= !col + size yytext;  ls := "NOT":: !ls   ; Tokens.NOT("NOT",!lin,!col));
"IMPLIES" => (col:= !col + size yytext; ls := "IMPLIES":: !ls   ;  Tokens.IMPLIES("IMPLIES",!lin,!col));
"EQUALS" => (col:= !col + size yytext;  ls := "EQUALS":: !ls ; Tokens.EQUALS("EQUALS",!lin,!col));
";"      => (col:= !col + size yytext;  ls := ";":: !ls    ;  Tokens.TERM(";",!lin,!col));
"("      => (col:= !col + size yytext;  ls := "(":: !ls    ;  Tokens.LPAREN("(",!lin,!col));
")"      => (col:= !col + size yytext;  ls := ")":: !ls    ;  Tokens.RPAREN(")",!lin,!col));
"TRUE"   => (col:= !col + size yytext;  ls := "CONST":: !ls ; Tokens.CONST("TRUE",!lin,!col));
"FALSE"  => (col:= !col + size yytext;  ls := "CONST":: !ls ; Tokens.CONST("FALSE",!lin,!col));
"IF"     => (col:= !col + size yytext;  ls := "IF":: !ls    ; Tokens.IF("IF",!lin,!col));
"ELSE"   => (col:= !col + size yytext;  ls := "ELSE":: !ls  ; Tokens.ELSE("ELSE",!lin,!col));
"THEN"   => (col:= !col + size yytext;  ls := "THEN":: !ls ;  Tokens.THEN("THEN",!lin,!col));
{alpha}+ => (col:= !col + size yytext;  ls := "ID":: !ls  ;  Tokens.ID(yytext,!lin,!col));
.        => (col:= !col + size yytext; ( iserror :=1 ; error(yytext,!lin,!col)); lex());







