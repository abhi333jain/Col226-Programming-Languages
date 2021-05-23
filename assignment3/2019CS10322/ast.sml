structure AST =
struct

type id = string

datatype binop = ADD | MINUS | TIMES | OR | AND| EQUALS | XOR | LESSTHAN | GREATERTHAN | IMPLIES

datatype unop = NOT | NEGATE

datatype typ = INT | BOOL| ARROW of typ*typ

datatype decl = ValDecl of id * exp | FunDecl of id * exp

and exp = NumExp of int
		| ConstExp of bool
    	| VarExp of id
    	| UnaryExp of unop * exp
		| BinExp of binop * exp * exp
		| LetExp of decl * exp
        | Ite of exp * exp * exp
        | Fn of id * typ * typ * exp
        | AppExp of exp * exp

and value = IntVal of int | BoolVal of bool | NULL | FnVal of id * typ * typ * exp
				      
datatype statement =  Expression of exp | Declaration of decl
datatype program = Program of statement*program | Statement of statement|EMPTY 

type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

exception Undefined_Variable of id

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail(var)	


type bound_environment = (id) list

fun nenvLookup (var:id, env:bound_environment) =
    case List.find(fn (x) => x = var) env of
				       SOME (x)   => false
				    |   NONE => true	

fun b_envAdd (var:id, env:bound_environment) =
    (var)::env

type t_environment = (id * typ) list

fun t_envAdd (var:id, v:typ, env:t_environment) =
    (var,v)::env

fun t_envLookup (var:id, env:t_environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail(var)	

val brokenTypes = Fail "Error in evaluation!"
			    

end


