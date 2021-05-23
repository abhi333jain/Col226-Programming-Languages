val arg=CommandLine.arguments() ;

structure CheckLrVals = CheckLrValsFun(structure Token = LrParser.Token) ;
structure CheckLex = CheckLexFun(structure Tokens = CheckLrVals.Tokens);
structure CheckParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CheckLrVals.ParserData
     	       structure Lex = CheckLex) ;
     
val ls = CheckLex.UserDeclarations.tokens_disp;

fun tokens_display ([]) = print("")
| tokens_display(hd::tl) = (print(hd^"\n"); tokens_display(tl));


fun error([],n) = print("")
| error([x],n) = (print( "statement -> formula TERM \n" ); OS.Process.exit(OS.Process.success))
| error(x::y::ls,n) =
    if((n=0) andalso not (x=";")) then (print( "statement -> formula TERM \n" ); OS.Process.exit(OS.Process.success))
   
    else if(y="ID" andalso x="ID") then ( print("formula -> formula AND formula\n" ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="ELSE" orelse y="THEN" orelse y="IF") andalso (not(x="ID" orelse x="CONST" orelse x="NOT"))) then ( print(" formula -> IF formula THEN formula ELSE formula \n" ); OS.Process.exit(OS.Process.success))
   
    else if ((y="IMPLIES") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="IMPLIES" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula IMPLIES formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="EQUALS") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="EQUALS" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula EQUALS formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="AND") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="AND" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula AND formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="OR") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="OR" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula OR formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="XOR") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="XOR" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula XOR formula \n"  ); OS.Process.exit(OS.Process.success)) 
     
     else if ((y="NOT") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF"))) then ( print("formula -> NOT formula \n"  ); OS.Process.exit(OS.Process.success)) 

    else if (x="(") then ( print("formula -> LPAREN formula  RPAREN \n"  ); OS.Process.exit(OS.Process.success)) 
    else error(y::ls,1)






fun invoke lexstream =
                let fun print_error (s,line:int,col:int) =
                ( TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString line) ^ ":" ^ (Int.toString col)^": " ) ; 
                    error(ls(),0));
        in
            CheckParser.parse(0,lexstream,print_error,())
        end ;
fun stringToLexer str =
    let val done = ref false
    	val lexer=  CheckParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	 ;
		
fun parse (lexer) =
    let val dumCheckEOF = CheckLrVals.Tokens.EOF(0,0)
    	val (res, lexer) = invoke lexer
	val (nextToken, lexer) = CheckParser.Stream.get lexer
    in
        if CheckParser.sameToken(nextToken, dumCheckEOF) then (res)
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); res)
    end ;



val instream = TextIO.openIn (hd(arg));
(*val str = TextIO.input instream ;*)


val lexer_lex = OnlyLex.makeLexer (fn (n) => TextIO.input instream) ;

fun generator() = 
    let
        val x= lexer_lex()
    in
        if(x=OnlyLex.UserDeclarations.EOF) then ()
        else generator()
    end ;

generator();


val instream1 = TextIO.openIn (hd(arg));
(*val str = TextIO.input instream1 ;*)
val parseString = parse o stringToLexer ;
print("[" ^ parseString(TextIO.input instream1 ) ^ "]" ^"\n");


