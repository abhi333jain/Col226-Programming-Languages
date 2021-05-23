functor CheckLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Check_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\117\000\002\000\117\000\003\000\117\000\004\000\117\000\
\\005\000\117\000\006\000\117\000\007\000\117\000\008\000\117\000\
\\009\000\117\000\011\000\117\000\012\000\117\000\013\000\117\000\
\\014\000\117\000\015\000\117\000\016\000\117\000\017\000\117\000\
\\018\000\117\000\019\000\117\000\020\000\117\000\021\000\117\000\
\\022\000\117\000\023\000\117\000\024\000\117\000\025\000\117\000\
\\026\000\117\000\028\000\117\000\031\000\117\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\006\000\027\000\
\\007\000\013\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\012\000\012\000\016\000\011\000\017\000\054\000\
\\018\000\022\000\019\000\021\000\020\000\020\000\021\000\010\000\
\\022\000\019\000\023\000\018\000\024\000\009\000\028\000\008\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\014\000\007\000\013\000\
\\012\000\012\000\016\000\011\000\021\000\010\000\024\000\009\000\
\\028\000\008\000\000\000\
\\001\000\004\000\030\000\000\000\
\\001\000\004\000\034\000\031\000\007\000\000\000\
\\001\000\004\000\050\000\000\000\
\\001\000\004\000\056\000\000\000\
\\001\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\013\000\055\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\022\000\019\000\023\000\018\000\000\000\
\\001\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\014\000\068\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\022\000\019\000\023\000\018\000\000\000\
\\001\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\015\000\078\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\022\000\019\000\023\000\018\000\000\000\
\\001\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\017\000\060\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\022\000\019\000\023\000\018\000\000\000\
\\001\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\022\000\019\000\023\000\018\000\026\000\067\000\000\000\
\\001\000\016\000\031\000\000\000\
\\001\000\016\000\049\000\000\000\
\\001\000\016\000\066\000\033\000\065\000\034\000\064\000\000\000\
\\001\000\017\000\071\000\032\000\070\000\000\000\
\\001\000\017\000\074\000\032\000\070\000\000\000\
\\001\000\017\000\077\000\032\000\070\000\000\000\
\\001\000\025\000\051\000\000\000\
\\001\000\027\000\052\000\000\000\
\\001\000\029\000\057\000\000\000\
\\001\000\029\000\062\000\000\000\
\\001\000\029\000\076\000\000\000\
\\001\000\029\000\079\000\000\000\
\\001\000\030\000\082\000\032\000\070\000\000\000\
\\001\000\030\000\083\000\032\000\070\000\000\000\
\\087\000\000\000\
\\088\000\003\000\016\000\004\000\015\000\005\000\014\000\007\000\013\000\
\\012\000\012\000\016\000\011\000\021\000\010\000\024\000\009\000\
\\028\000\008\000\031\000\007\000\000\000\
\\089\000\003\000\016\000\004\000\015\000\005\000\014\000\007\000\013\000\
\\012\000\012\000\016\000\011\000\021\000\010\000\024\000\009\000\
\\028\000\008\000\031\000\007\000\000\000\
\\090\000\000\000\
\\091\000\002\000\028\000\006\000\027\000\008\000\026\000\009\000\025\000\
\\010\000\024\000\011\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\022\000\019\000\023\000\018\000\000\000\
\\092\000\000\000\
\\093\000\002\000\017\000\000\000\
\\094\000\000\000\
\\095\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\022\000\019\000\023\000\018\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\032\000\070\000\000\000\
\\101\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\022\000\019\000\023\000\018\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\010\000\024\000\000\000\
\\106\000\010\000\024\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\010\000\024\000\020\000\020\000\000\000\
\\110\000\010\000\024\000\020\000\020\000\000\000\
\\111\000\010\000\024\000\000\000\
\\112\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\000\000\
\\113\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\000\000\
\\114\000\010\000\024\000\020\000\020\000\000\000\
\\115\000\010\000\024\000\020\000\020\000\000\000\
\\116\000\010\000\024\000\020\000\020\000\000\000\
\\118\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\006\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\011\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\022\000\019\000\023\000\018\000\000\000\
\"
val actionRowNumbers =
"\028\000\033\000\031\000\029\000\
\\027\000\004\000\013\000\005\000\
\\003\000\003\000\003\000\043\000\
\\003\000\044\000\042\000\034\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\032\000\030\000\
\\014\000\006\000\036\000\019\000\
\\020\000\046\000\002\000\008\000\
\\045\000\053\000\052\000\051\000\
\\050\000\049\000\057\000\001\000\
\\056\000\055\000\054\000\007\000\
\\021\000\003\000\003\000\011\000\
\\048\000\003\000\022\000\015\000\
\\012\000\035\000\047\000\009\000\
\\015\000\016\000\038\000\037\000\
\\015\000\059\000\003\000\017\000\
\\015\000\023\000\018\000\010\000\
\\024\000\040\000\015\000\039\000\
\\058\000\015\000\025\000\026\000\
\\003\000\003\000\060\000\041\000\
\\000\000"
val gotoT =
"\
\\001\000\084\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\027\000\003\000\003\000\004\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\031\000\007\000\030\000\000\000\
\\004\000\033\000\000\000\
\\004\000\034\000\000\000\
\\004\000\035\000\000\000\
\\000\000\
\\004\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\037\000\000\000\
\\004\000\038\000\000\000\
\\004\000\039\000\000\000\
\\004\000\040\000\000\000\
\\004\000\041\000\000\000\
\\004\000\042\000\000\000\
\\004\000\043\000\000\000\
\\004\000\044\000\000\000\
\\004\000\045\000\000\000\
\\004\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\051\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\056\000\000\000\
\\004\000\057\000\000\000\
\\000\000\
\\000\000\
\\004\000\059\000\000\000\
\\000\000\
\\006\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\070\000\000\000\
\\000\000\
\\004\000\071\000\000\000\
\\000\000\
\\006\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\078\000\000\000\
\\000\000\
\\000\000\
\\006\000\079\000\000\000\
\\000\000\
\\000\000\
\\004\000\082\000\000\000\
\\004\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 85
val numrules = 35
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | BOOL of unit ->  (string) | INT of unit ->  (string)
 | ARROW of unit ->  (string) | FUN of unit ->  (string)
 | IMP of unit ->  (string) | COLON of unit ->  (string)
 | FN of unit ->  (string) | EQ of unit ->  (string)
 | END of unit ->  (string) | IN of unit ->  (string)
 | LET of unit ->  (string) | GREATERTHAN of unit ->  (string)
 | LESSTHAN of unit ->  (string) | NEGATE of unit ->  (string)
 | TIMES of unit ->  (string) | MINUS of unit ->  (string)
 | PLUS of unit ->  (string) | RPAREN of unit ->  (string)
 | LPAREN of unit ->  (string) | FI of unit ->  (string)
 | ELSE of unit ->  (string) | THEN of unit ->  (string)
 | IF of unit ->  (string) | IMPLIES of unit ->  (string)
 | EQUALS of unit ->  (string) | XOR of unit ->  (string)
 | OR of unit ->  (string) | NUM of unit ->  (int)
 | AND of unit ->  (string) | NOT of unit ->  (string)
 | ID of unit ->  (string) | CONST of unit ->  (bool)
 | TERM of unit ->  (string) | fundef of unit ->  (AST.decl)
 | fntype of unit ->  (AST.typ) | decl of unit ->  (AST.decl)
 | exp of unit ->  (AST.exp) | statement of unit ->  (AST.statement)
 | program of unit ->  (AST.program) | start of unit ->  (AST.program)
end
type svalue = MlyValue.svalue
type result = AST.program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "TERM"
  | (T 2) => "CONST"
  | (T 3) => "ID"
  | (T 4) => "NOT"
  | (T 5) => "AND"
  | (T 6) => "NUM"
  | (T 7) => "OR"
  | (T 8) => "XOR"
  | (T 9) => "EQUALS"
  | (T 10) => "IMPLIES"
  | (T 11) => "IF"
  | (T 12) => "THEN"
  | (T 13) => "ELSE"
  | (T 14) => "FI"
  | (T 15) => "LPAREN"
  | (T 16) => "RPAREN"
  | (T 17) => "PLUS"
  | (T 18) => "MINUS"
  | (T 19) => "TIMES"
  | (T 20) => "NEGATE"
  | (T 21) => "LESSTHAN"
  | (T 22) => "GREATERTHAN"
  | (T 23) => "LET"
  | (T 24) => "IN"
  | (T 25) => "END"
  | (T 26) => "EQ"
  | (T 27) => "FN"
  | (T 28) => "COLON"
  | (T 29) => "IMP"
  | (T 30) => "FUN"
  | (T 31) => "ARROW"
  | (T 32) => "INT"
  | (T 33) => "BOOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.start (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)

end
|  ( 1, ( rest671)) => let val  result = MlyValue.start (fn _ => (
AST.EMPTY))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statement as statement1) = statement1 ()
 in (AST.Statement(statement))
end)
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.program program1, _, program1right)) :: ( _,
 ( MlyValue.statement statement1, statement1left, _)) :: rest671)) =>
 let val  result = MlyValue.program (fn _ => let val  (statement as 
statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in (AST.Program(statement,program))
end)
 in ( LrTable.NT 1, ( result, statement1left, program1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.statement (fn _ => let val  (exp as 
exp1) = exp1 ()
 in (AST.Expression(exp))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.TERM TERM1, _, TERM1right)) :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (exp as exp1) = exp1 ()
 val  TERM1 = TERM1 ()
 in (AST.Expression(exp))
end)
 in ( LrTable.NT 2, ( result, exp1left, TERM1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.fundef fundef1, fundef1left, fundef1right))
 :: rest671)) => let val  result = MlyValue.statement (fn _ => let
 val  (fundef as fundef1) = fundef1 ()
 in (AST.Declaration(fundef))
end)
 in ( LrTable.NT 2, ( result, fundef1left, fundef1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.TERM TERM1, _, TERM1right)) :: ( _, ( 
MlyValue.fundef fundef1, fundef1left, _)) :: rest671)) => let val  
result = MlyValue.statement (fn _ => let val  (fundef as fundef1) = 
fundef1 ()
 val  TERM1 = TERM1 ()
 in (AST.Declaration(fundef))
end)
 in ( LrTable.NT 2, ( result, fundef1left, TERM1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.EQ EQ1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.decl (fn _ => let val  (ID as 
ID1) = ID1 ()
 val  EQ1 = EQ1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.ValDecl(ID, exp))
end)
 in ( LrTable.NT 4, ( result, ID1left, exp1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.fundef fundef1, fundef1left, fundef1right))
 :: rest671)) => let val  result = MlyValue.decl (fn _ => let val  (
fundef as fundef1) = fundef1 ()
 in (fundef)
end)
 in ( LrTable.NT 4, ( result, fundef1left, fundef1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.fntype (fn _ => let val  (INT as INT1)
 = INT1 ()
 in (AST.INT)
end)
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.fntype (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in (AST.BOOL)
end)
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.fntype fntype1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.fntype (fn
 _ => let val  LPAREN1 = LPAREN1 ()
 val  (fntype as fntype1) = fntype1 ()
 val  RPAREN1 = RPAREN1 ()
 in (fntype)
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.fntype fntype2, _, fntype2right)) :: ( _, (
 MlyValue.ARROW ARROW1, _, _)) :: ( _, ( MlyValue.fntype fntype1, 
fntype1left, _)) :: rest671)) => let val  result = MlyValue.fntype (fn
 _ => let val  fntype1 = fntype1 ()
 val  (ARROW as ARROW1) = ARROW1 ()
 val  fntype2 = fntype2 ()
 in (AST.ARROW(fntype1,fntype2))
end)
 in ( LrTable.NT 5, ( result, fntype1left, fntype2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.IMP IMP1, _, _)) :: ( _, ( MlyValue.fntype fntype2, _, _)) ::
 ( _, ( MlyValue.COLON COLON2, _, _)) :: ( _, ( MlyValue.RPAREN 
RPAREN1, _, _)) :: ( _, ( MlyValue.fntype fntype1, _, _)) :: ( _, ( 
MlyValue.COLON COLON1, _, _)) :: ( _, ( MlyValue.ID ID2, _, _)) :: ( _
, ( MlyValue.LPAREN LPAREN1, _, _)) :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( MlyValue.FUN FUN1, FUN1left, _)) :: rest671)) => let val  
result = MlyValue.fundef (fn _ => let val  FUN1 = FUN1 ()
 val  ID1 = ID1 ()
 val  LPAREN1 = LPAREN1 ()
 val  ID2 = ID2 ()
 val  COLON1 = COLON1 ()
 val  fntype1 = fntype1 ()
 val  RPAREN1 = RPAREN1 ()
 val  COLON2 = COLON2 ()
 val  fntype2 = fntype2 ()
 val  IMP1 = IMP1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.FunDecl((ID1),AST.Fn(ID2,fntype1,fntype2,exp)))
end)
 in ( LrTable.NT 6, ( result, FUN1left, exp1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (AST.ConstExp(CONST))
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.NOT NOT1, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (NOT as NOT1) = NOT1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.UnaryExp(AST.NOT,exp))
end)
 in ( LrTable.NT 3, ( result, NOT1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.NEGATE NEGATE1, NEGATE1left, _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (NEGATE as NEGATE1) = NEGATE1
 ()
 val  (exp as exp1) = exp1 ()
 in (AST.UnaryExp(AST.NEGATE,exp))
end)
 in ( LrTable.NT 3, ( result, NEGATE1left, exp1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.exp exp2, _, _)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _,
 ( MlyValue.LPAREN LPAREN1, LPAREN1left, _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  LPAREN1 = LPAREN1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  RPAREN1 = RPAREN1 ()
 in (AST.AppExp(exp1,exp2))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.exp exp1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  LPAREN1 = LPAREN1 ()
 val  (exp as exp1) = exp1 ()
 val  RPAREN1 = RPAREN1 ()
 in (exp)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.PLUS PLUS1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  PLUS1 = PLUS1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.ADD, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.MINUS MINUS1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _
)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  (MINUS as MINUS1) = MINUS1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.MINUS, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.TIMES TIMES1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _
)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  (TIMES as TIMES1) = TIMES1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.TIMES, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.LESSTHAN LESSTHAN1, _, _)) :: ( _, ( MlyValue.exp exp1, 
exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  exp1 = exp1 ()
 val  (LESSTHAN as LESSTHAN1) = LESSTHAN1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.LESSTHAN, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.GREATERTHAN GREATERTHAN1, _, _)) :: ( _, ( MlyValue.exp exp1,
 exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  exp1 = exp1 ()
 val  (GREATERTHAN as GREATERTHAN1) = GREATERTHAN1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.GREATERTHAN, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.AND AND1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  (AND as AND1) = AND1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.AND, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.OR OR1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  (OR as OR1) = OR1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.OR, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.XOR XOR1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  (XOR as XOR1) = XOR1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.XOR, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.EQUALS EQUALS1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left,
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  (EQUALS as EQUALS1) = EQUALS1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.EQUALS, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.IMPLIES IMPLIES1, _, _)) :: ( _, ( MlyValue.exp exp1, 
exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  exp1 = exp1 ()
 val  (IMPLIES as IMPLIES1) = IMPLIES1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.IMPLIES, exp1,  exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.FI FI1, _, FI1right)) :: ( _, ( 
MlyValue.exp exp3, _, _)) :: ( _, ( MlyValue.ELSE ELSE1, _, _)) :: ( _
, ( MlyValue.exp exp2, _, _)) :: ( _, ( MlyValue.THEN THEN1, _, _)) ::
 ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( MlyValue.IF IF1, IF1left,
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
IF1 = IF1 ()
 val  exp1 = exp1 ()
 val  THEN1 = THEN1 ()
 val  exp2 = exp2 ()
 val  ELSE1 = ELSE1 ()
 val  exp3 = exp3 ()
 val  FI1 = FI1 ()
 in (AST.Ite(exp1, exp2, exp3) )
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.END END1, _, END1right)) :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( MlyValue.IN IN1, _, _)) :: ( _, ( 
MlyValue.decl decl1, _, _)) :: ( _, ( MlyValue.LET LET1, LET1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  LET1
 = LET1 ()
 val  (decl as decl1) = decl1 ()
 val  IN1 = IN1 ()
 val  (exp as exp1) = exp1 ()
 val  END1 = END1 ()
 in (AST.LetExp(decl, exp))
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.IMP IMP1, _, _)) :: ( _, ( MlyValue.fntype fntype2, _, _)) ::
 ( _, ( MlyValue.COLON COLON2, _, _)) :: ( _, ( MlyValue.RPAREN 
RPAREN1, _, _)) :: ( _, ( MlyValue.fntype fntype1, _, _)) :: ( _, ( 
MlyValue.COLON COLON1, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _
, ( MlyValue.LPAREN LPAREN1, _, _)) :: ( _, ( MlyValue.FN FN1, FN1left
, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val 
 FN1 = FN1 ()
 val  LPAREN1 = LPAREN1 ()
 val  (ID as ID1) = ID1 ()
 val  COLON1 = COLON1 ()
 val  fntype1 = fntype1 ()
 val  RPAREN1 = RPAREN1 ()
 val  COLON2 = COLON2 ()
 val  fntype2 = fntype2 ()
 val  IMP1 = IMP1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.Fn(ID,fntype1,fntype2,exp))
end)
 in ( LrTable.NT 3, ( result, FN1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Check_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.TERM (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NOT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.NOT (fn () => i),p1,p2))
fun AND (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.AND (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun OR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.OR (fn () => i),p1,p2))
fun XOR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.XOR (fn () => i),p1,p2))
fun EQUALS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.EQUALS (fn () => i),p1,p2))
fun IMPLIES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.IMPLIES (fn () => i),p1,p2))
fun IF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.IF (fn () => i),p1,p2))
fun THEN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.THEN (fn () => i),p1,p2))
fun ELSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.ELSE (fn () => i),p1,p2))
fun FI (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.FI (fn () => i),p1,p2))
fun LPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.LPAREN (fn () => i),p1,p2))
fun RPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.RPAREN (fn () => i),p1,p2))
fun PLUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.PLUS (fn () => i),p1,p2))
fun MINUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.MINUS (fn () => i),p1,p2))
fun TIMES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.TIMES (fn () => i),p1,p2))
fun NEGATE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.NEGATE (fn () => i),p1,p2))
fun LESSTHAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.LESSTHAN (fn () => i),p1,p2))
fun GREATERTHAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.GREATERTHAN (fn () => i),p1,p2))
fun LET (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.LET (fn () => i),p1,p2))
fun IN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.IN (fn () => i),p1,p2))
fun END (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.END (fn () => i),p1,p2))
fun EQ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.EQ (fn () => i),p1,p2))
fun FN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.FN (fn () => i),p1,p2))
fun COLON (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.COLON (fn () => i),p1,p2))
fun IMP (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.IMP (fn () => i),p1,p2))
fun FUN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.FUN (fn () => i),p1,p2))
fun ARROW (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.ARROW (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
end
end
