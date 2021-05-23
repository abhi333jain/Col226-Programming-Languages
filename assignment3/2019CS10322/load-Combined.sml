val arg=CommandLine.arguments() ;

structure CheckLrVals = CheckLrValsFun(structure Token = LrParser.Token) ;
structure CheckLex = CheckLexFun(structure Tokens = CheckLrVals.Tokens);
structure CheckParser =
      Join(structure LrParser = LrParser
               structure ParserData = CheckLrVals.ParserData
               structure Lex = CheckLex) ;
     
val ls = CheckLex.UserDeclarations.tokens_disp;

(*fun tokens_display ([]) = print("")
| tokens_display(hd::tl) = (print(hd^"\n"); tokens_display(tl));


fun error([],n) = print("")
| error([x],n) = (print( "statement -> formula TERM \n" ); OS.Process.exit(OS.Process.success))
| error(x::y::ls,n) =
    if((n=0) andalso (not (x=";") andalso ((x="CONST") orelse (x="ID")))) then (print( "statement -> formula TERM \n" ); OS.Process.exit(OS.Process.success))
   
    else if((y="ID" orelse y="CONST") andalso (x="ID" orelse x="CONST")) then ( print("formula -> formula AND formula\n" ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="ELSE" orelse y="THEN" orelse y="IF") andalso (not(x="ID" orelse x="CONST" orelse x="NOT"))) then ( print(" formula -> IF formula THEN formula ELSE formula \n" ); OS.Process.exit(OS.Process.success))
   
    else if ((y="IMPLIES") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="IMPLIES" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula IMPLIES formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="EQUALS") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="EQUALS" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula EQUALS formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="AND") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="AND" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula AND formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="OR") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="OR" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula OR formula \n"  ); OS.Process.exit(OS.Process.success)) 
   
    else if ((y="XOR") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF")) orelse (x="XOR" andalso (not (y="ID" orelse y="CONST")))) then ( print("formula -> formula XOR formula \n"  ); OS.Process.exit(OS.Process.success)) 
     
     else if ((y="NOT") andalso (not(x="ID" orelse x="CONST" orelse x="NOT" orelse x="IF"))) then ( print("formula -> NOT formula \n"  ); OS.Process.exit(OS.Process.success)) 

    else if (x="(") then ( print("formula -> LPAREN formula  RPAREN \n"  ); OS.Process.exit(OS.Process.success)   ) 
    else error(y::ls,1)
*)





fun invoke lexstream =
                let fun print_error (s,line:int,col:int) =
                ( TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString line) ^ ":" ^ (Int.toString col)^": " ); OS.Process.exit(OS.Process.success));
        in
            CheckParser.parse(0,lexstream,print_error,())
        end ;
fun stringToLexer str =
    let val done = ref false
        val lexer=  CheckParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
    lexer
    end  ;
        
fun parse (lexer) =
    let val dumCheckEOF = CheckLrVals.Tokens.EOF(0,0)
        val (res, lexer) = invoke lexer
    val (nextToken, lexer) = CheckParser.Stream.get lexer
    in
        if CheckParser.sameToken(nextToken, dumCheckEOF) then (res)
    else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); res)
    end ;





open AST

fun toStringExp(e : exp) =
    case e of
         NumExp i           => "NumExp " ^Int.toString(i)
      | ConstExp s          => "ConstExp "^ Bool.toString(s)
      | VarExp x            => "VarExp \""^x^"\""               
      | BinExp (b, e1, e2)  => "BinExp (" ^ toStringBinExp(b,e1,e2) ^")" 
      | UnaryExp(b,e)       => "UnaryExp (" ^ toStringUnaryExp(b,e) ^ ")"
      | Ite(cond,e1,e2)     => "ITE (" ^ toStringITE(cond,e1,e2) ^")"
      | Fn(id,typ1,typ2,e)  => "Fn (\"" ^ id ^"\"," ^toStringType(typ1)^","^toStringType(typ2)^","^toStringExp(e) ^")"
      | AppExp(f,e)        => "AppExp (" ^ toStringExp(f)^ ","^toStringExp(e) ^")"
      | LetExp(ValDecl(x,e1), e2)  =>
            let
                val str1 = toStringExp(e1)
                val str2 = toStringExp(e2) 
            in
                "LetExp (ValDecl (\""^x^"\","^str1^"),"^str2^")"
            end
       | LetExp(FunDecl(x,e1),e2) =>
            let
                val str1 = toStringExp(e1)
                val str2 = toStringExp(e2) 
            in
                "LetExp (FunDecl (\""^x^"\","^str1^"),"^str2^")"
            end


and

toStringBinExp(b:binop, e1:exp, e2:exp) =
    let 
        val str1 = toStringExp(e1)
        val str2 = toStringExp(e2)
    in
        case b  of
              ADD         => "ADD,"^str1^","^str2
          |   MINUS       => "MINUS,"^str1^","^str2
          |   TIMES       => "TIMES,"^str1^","^str2
          |   EQUALS      => "EQUALS,"^str1^","^str2
          |   AND         => "AND,"^str1^","^str2
          |   OR          => "OR,"^str1^","^str2
          |   XOR         => "XOR,"^str1^","^str2
          |   IMPLIES     => "IMPLIES,"^str1^","^str2
          |   LESSTHAN    => "LESSTHAN,"^str1^","^str2
          |   GREATERTHAN => "GREATERTHAN,"^str1^","^str2
    end

and

toStringUnaryExp(b:unop, e1:exp) =
    let 
        val str1 = toStringExp(e1)
    in
        case b  of
              NEGATE => "NEGATE,"^str1
          |   NOT    => "NOT,"^str1
    end     

and

toStringITE(cond:exp, e1:exp, e2:exp) = 
      let 
        val str1 = toStringExp(cond)
        val str2 = toStringExp(e1)
        val str3 = toStringExp(e2)
      in
        str1^","^str2^","^str3
      end

and

toStringType(t : typ) = 
    case (t)  of
          INT          => "INT"
      |   BOOL         =>  "BOOL"
      |   ARROW(t1,t2) => "ARROW ("^toStringType(t1)^","^toStringType(t2)^")"


and

toStringStatement(s : statement) = 
    case s of 
        Expression e => "Expression ("^toStringExp(e)^")"
       | Declaration(ValDecl(x,e1))  =>
            let
                val str1 = toStringExp(e1)
            in
                "Declaration (ValDecl (\""^x^"\","^str1^")"
            end
       | Declaration(FunDecl(x,e1)) =>
            let
                val str1 = toStringExp(e1)
            in
                "Declaration (FunDecl (\""^x^"\","^str1^")"
            end

and

toStringProgram(p : program) = 
    case p of
        Statement(s) => "Statement ("^toStringStatement(s)^")"
       | Program (s,p1) => "Program ("^toStringStatement(s) ^ ","^toStringProgram(p1)^")"



open TYPECHECKER

fun toStringTypeList ([]) = "\b"
| toStringTypeList(hd::tl) = 
    let
        val str=toStringTypeList(tl);
    in
        toStringType(hd)^","^str
    end



open EVALUATOR

fun toStringVal(e :value) =
    case e of
        IntVal i => "IntVal "^Int.toString(i)
       |BoolVal i => "BoolVal "^Bool.toString(i)
       |FnVal(id,typ1,typ2,exp) => "FnVal (\"" ^ id ^"\"," ^toStringType(typ1)^","^toStringType(typ2)^","^toStringExp(exp) ^")"

fun toStringValList ([]) = "\b"
| toStringValList(hd::tl) = 
    let
        val str=toStringValList(tl);
    in
        toStringVal(hd)^","^str
    end


val instream1 = TextIO.openIn (hd(arg));
val str = TextIO.input instream1 ;

val parseString = parse o stringToLexer ;


val ast = parseString(str);

print( toStringProgram(ast) ^"\n"^"\n" ); 

fun typecall () = print("["^toStringTypeList(TYPECHECKER.typeProgram(ast))^"]"^"\n"^"\n")
                  handle Type_Error(typ1,typ2,exp) =>  ( print( "Type error in expression \n"^toStringExp(exp)^"\nFound type : "^ toStringType(typ1)^"\nExpected type : "^toStringType(typ2)^" \n \n") ; OS.Process.exit(OS.Process.success) ) 
                        | Fail(x) => ( print("Undefined variable : "^x^"\n \n"); OS.Process.exit(OS.Process.success))
                        | Equality_Error(typ1,typ2,exp) => ( print( "Type error in expression \n"^toStringExp(exp)^"\nCan't compare the types : "^ toStringType(typ1)^" and "^toStringType(typ2)^" \n \n") ; OS.Process.exit(OS.Process.success) ); 

typecall();

print("["^toStringValList(EVALUATOR.evalProgram(ast))^"]"^"\n"^"\n");






