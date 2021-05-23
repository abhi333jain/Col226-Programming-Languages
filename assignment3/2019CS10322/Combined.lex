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

fun revfold _ nil b = b
| revfold f (hd::tl) b = revfold f tl (f(hd,b))

  
  val keywords =
  [
   ("AND",  Tokens.AND),
   ("OR",  Tokens.OR),
   ("XOR",  Tokens.XOR),
   ("NOT",  Tokens.NOT),
   ("NEGATE",  Tokens.NEGATE),
   ("PLUS",  Tokens.PLUS),
   ("MINUS",  Tokens.MINUS),
   ("TIMES",  Tokens.TIMES),
   ("IMPLIES",  Tokens.IMPLIES),
   ("EQUALS",  Tokens.EQUALS),
   ("GREATERTHAN",  Tokens.GREATERTHAN),
   ("LESSTHAN",  Tokens.LESSTHAN),
   ("let",  Tokens.LET),
   ("in",  Tokens.IN),
   ("end",  Tokens.END),
   ("fn",  Tokens.FN),
   ("fun",  Tokens.FUN),
   ("if",  Tokens.IF),
   ("then",  Tokens.THEN),
   ("else",  Tokens.ELSE),
   ("fi",  Tokens.FI),
   ("int",Tokens.INT),
   ("bool",Tokens.BOOL)
   ]

  fun findKeywords (str:string, pos1:pos, pos2:pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => (ls := str :: !ls ; tk(str,pos1, pos2))
  | NONE => (ls := "ID" :: !ls ; Tokens.ID (str, pos1, pos2))



%%
%header (functor CheckLexFun(structure Tokens:Check_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
digit=[0-9];

%%

\n       => (lin:=(!lin)+1; col:=0 ;lex() );
\r\n       => (lin:=(!lin)+1; col:=0 ;lex() );
{ws}+    => (col:= !col + size yytext; lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !lin,!col));

";"      => (col:= !col + size yytext;  ls := ";":: !ls    ;  Tokens.TERM(";",!lin,!col));
"("      => (col:= !col + size yytext;  ls := "(":: !ls    ;  Tokens.LPAREN("(",!lin,!col));
")"      => (col:= !col + size yytext;  ls := ")":: !ls    ;  Tokens.RPAREN(")",!lin,!col));
"="      => (col:= !col + size yytext;  ls := "=":: !ls    ;  Tokens.EQ("=",!lin,!col));
"=>"     => (col:= !col + size yytext;  ls := "=>":: !ls    ;  Tokens.IMP("=>",!lin,!col));
"->"     => (col:= !col + size yytext;  ls := "->":: !ls    ;  Tokens.ARROW("->",!lin,!col));
":"      => (col:= !col + size yytext;  ls := ":":: !ls    ;  Tokens.COLON(":",!lin,!col));
"TRUE"   => (col:= !col + size yytext;  ls := "CONST":: !ls ; Tokens.CONST(true,!lin,!col));
"FALSE"  => (col:= !col + size yytext;  ls := "CONST":: !ls ; Tokens.CONST(false,!lin,!col));
{alpha}+{digit}* => (col:= !col + size yytext;  findKeywords(yytext,!lin,!col) );
.        => (col:= !col + size yytext; ( iserror :=1 ; error(yytext,!lin,!col)); lex());







