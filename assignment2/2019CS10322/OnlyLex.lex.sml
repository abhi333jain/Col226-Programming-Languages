structure OnlyLex=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "OnlyLex.lex"*)datatype lexresult= EOF| TERM of string| CONST of string| ID of string | NOT of string| AND of string| 
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

(*#line 19.1 "OnlyLex.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\009\011\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\009\003\003\003\003\003\003\003\008\007\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\006\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 15)], trans = 0},
{fin = [(N 13),(N 15)], trans = 4},
{fin = [(N 13)], trans = 4},
{fin = [(N 6),(N 15)], trans = 0},
{fin = [(N 10),(N 15)], trans = 0},
{fin = [(N 8),(N 15)], trans = 0},
{fin = [(N 4),(N 15)], trans = 9},
{fin = [(N 4)], trans = 9},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((*#line 23.14 "OnlyLex.lex"*)lin:=(!lin)+1;  col:=0 ;lex() (*#line 136.1 "OnlyLex.lex.sml"*)
)
| 10 => let val yytext=yymktext() in (*#line 27.14 "OnlyLex.lex"*)col:= !col + size yytext; str := !str^"RPAREN \""^yytext^"\", ";  RPAREN(")")(*#line 138.1 "OnlyLex.lex.sml"*)
 end
| 13 => let val yytext=yymktext() in (*#line 28.14 "OnlyLex.lex"*)col:= !col + size yytext;  
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
                (*#line 153.1 "OnlyLex.lex.sml"*)
 end
| 15 => let val yytext=yymktext() in (*#line 42.14 "OnlyLex.lex"*)col:= !col + size yytext; ( iserror :=1 ; error(yytext,!lin,!col)); lex()(*#line 155.1 "OnlyLex.lex.sml"*)
 end
| 4 => let val yytext=yymktext() in (*#line 24.14 "OnlyLex.lex"*)col:= !col + size yytext; lex()(*#line 157.1 "OnlyLex.lex.sml"*)
 end
| 6 => let val yytext=yymktext() in (*#line 25.14 "OnlyLex.lex"*)col:= !col + size yytext; str := !str^"TERM \""^yytext^"\", ";  TERM(";")(*#line 159.1 "OnlyLex.lex.sml"*)
 end
| 8 => let val yytext=yymktext() in (*#line 26.14 "OnlyLex.lex"*)col:= !col + size yytext; str := !str^"LPAREN \""^yytext^"\", "; LPAREN("(")(*#line 161.1 "OnlyLex.lex.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
