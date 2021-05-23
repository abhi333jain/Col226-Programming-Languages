structure TYPECHECKER  =
struct
open AST

exception Type_Error of typ*typ*exp
exception Equality_Error of typ*typ*exp

fun typeExp(expr:exp, env:t_environment) : typ =
    case expr of
	     NumExp i            => INT
      | ConstExp s         => BOOL
      | VarExp x            => t_envLookup (x, env)				  
      | BinExp (b, e1, e2)  => typeBinExp(b, e1, e2, env)
      | UnaryExp(b,e) =>  typeUnaryExp(b,e,env)
      | Ite(cond,e1,e2) => typeITE(cond,e1,e2,env)
      | Fn(id,typ1,typ2,e) =>
      	let
      		val x = typeExp(e, t_envAdd (id, typ1, env)) 
      	in 
      		if(x=typ2) then ARROW(typ1,typ2)
            else raise Type_Error(x,typ2,e)
        end  
      | AppExp(f,e) => 
        let
          val arg = typeExp (e, env)  
          val functype= typeExp (f, env)
         (* val ARROW(typ1,typ2)= typeExp (f, env)  *)
        in
          case functype of
            ARROW(typ1,typ2) =>	if(arg=typ1) then typ2
                                  else raise Type_Error(arg,typ1,e)
            | _ => raise Type_Error(functype,arg,f)
        end           
      | LetExp(ValDecl(x,e1), e2)  =>
          let
      	    val v1 = typeExp (e1, env)
      	  in
      	   typeExp(e2, t_envAdd (x, v1, env))
          end
       | LetExp(FunDecl(x,e1),e2) =>
          let
            val v1 = typeExp (e1, env)
          in
            typeExp(e2, t_envAdd (x, v1, env))
          
        end		   
and

typeBinExp(b:binop, e1:exp, e2:exp, env:t_environment):typ =
case (b, typeExp(e1, env), typeExp(e2, env))  of
      (ADD, INT , INT ) => INT
  |   (MINUS, INT , INT ) => INT 
  |   (TIMES, INT , INT ) => INT 
  |   (EQUALS, INT , INT )  => BOOL 
  |   (EQUALS, BOOL , BOOL )  => BOOL 
  |   (AND, BOOL , BOOL ) => BOOL 
  |   (OR, BOOL , BOOL ) => BOOL 
  |   (XOR, BOOL , BOOL ) => BOOL 
  |   (IMPLIES, BOOL , BOOL ) => BOOL 
  |   (LESSTHAN, INT , INT ) => BOOL 
  |   (GREATERTHAN, INT , INT ) => BOOL 
  |   (EQUALS,_,_) => raise Equality_Error(typeExp(e1, env),typeExp(e2, env),BinExp (b, e1, e2))
  |   _ => raise Type_Error(typeExp(e1, env),typeExp(e2, env),BinExp (b, e1, e2))

and 

typeUnaryExp(b:unop, e1:exp, env:t_environment):typ =
case (b, typeExp(e1, env))  of
      (NEGATE, INT ) => INT
  |   (NOT, BOOL ) => BOOL 
  |   (NEGATE,_)  => raise Type_Error(typeExp(e1, env),INT,UnaryExp(b,e1))  
  |   (NOT,_)  => raise Type_Error(typeExp(e1, env),BOOL,UnaryExp(b,e1))
and

typeITE(cond:exp, e1:exp, e2:exp,env:t_environment):typ = 
      let 
          val x = typeExp(cond,env)
          val y = typeExp(e1,env)
          val z = typeExp(e2,env)
      in
        case x of 
          BOOL=> ( if(y=z) then y else raise Type_Error(y,z,e2))
          |   _  => raise Type_Error(x,BOOL,cond)
      end

fun augment(env,fname,Fn(id,typ1,typ2,e)):t_environment = t_envAdd(fname,ARROW(typ1,typ2),env)

fun typeProgram(p : program) = 
  let

    fun iter(p : program,env: t_environment,ls) =      
      case p of EMPTY => []
         | Statement(Expression (s)) => ( ls @ [typeExp(s,env)] )
         | Statement(Declaration(FunDecl(id,exp)))=> ( ls @ [ typeExp( exp,t_envAdd(id,typeExp(exp,augment(env,id,exp)),env) ) ] )
         | Program (Declaration(FunDecl(id,exp)),pr) => ( iter( pr, t_envAdd(id,typeExp(exp,augment(env,id,exp)),env) , ls @ [ typeExp( exp,t_envAdd(id,typeExp(exp,augment(env,id,exp)),env) ) ] ) )
         | Program (Expression (s),pr) =>(  iter(pr,env,ls @ [typeExp(s,env)]) )
  in 
    iter(p,[],[])
  end
end;







