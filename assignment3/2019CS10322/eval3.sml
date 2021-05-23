structure EVALUATOR  =
struct
open AST


fun evalExp(e:exp, env:environment):value =
    case e of
       NumExp i            => IntVal i
      | ConstExp s         => BoolVal s
      | VarExp x            => envLookup (x, env)           
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UnaryExp(b,e) =>  evalUnaryExp(b,e,env)
      | Ite(cond,e1,e2) => evalITE(cond,e1,e2,env)
      | Fn(id,typ1,typ2,e) => FnVal(id,typ1,typ2,replaceFree(e,[id],env))
      | AppExp(f,e) => 
        let
          val arg = evalExp (e, env)  
          val FnVal(id,typ1,typ2,e1)= evalExp (f, env) 
        in
            evalExp(replaceFree(e1,[],envAdd(id,arg,env)),envAdd(id,arg,env))
        end           
      | LetExp(ValDecl(x,e1), e2)  =>
          let
            val v1 = evalExp (e1, env)
          in
            evalExp(e2, envAdd (x, v1, env))
          end
       | LetExp(FunDecl(x,e1),e2) =>
          let
            val v1 = evalExp (e1, env)
          in
            evalExp(e2, envAdd (x, v1, env))
          
        end      
and

evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
      (ADD, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (MINUS, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (TIMES, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (EQUALS, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
  |   (EQUALS, BoolVal i1, BoolVal i2)  => BoolVal (i1 = i2) (* ((i1 andalso i2) OR ( not(i1) andalso not(i2))) *)
  |   (AND, BoolVal i1, BoolVal i2) => BoolVal (i1 andalso i2)
  |   (OR, BoolVal i1, BoolVal i2) => BoolVal (i1 orelse i2)
  |   (XOR, BoolVal i1, BoolVal i2) => BoolVal ((i1 andalso (not i2)) orelse (i2 andalso (not i1)))
  |   (IMPLIES, BoolVal i1, BoolVal i2) => BoolVal ( (not i1) orelse i2)
  |   (LESSTHAN, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
  |   (GREATERTHAN, IntVal i1, IntVal i2) => BoolVal (i1 > i2)
  |   _  => raise brokenTypes 

and 

evalUnaryExp(b:unop, e1:exp, env:environment):value =
case (b, evalExp(e1, env))  of
      (NEGATE, IntVal i) => IntVal(~i)
  |   (NOT, BoolVal i) => BoolVal (not i )
  |   _  => raise brokenTypes     

and

evalITE(cond:exp, e1:exp, e2:exp,env:environment):value = 
      let 
          val x = evalExp(cond,env)
      in
        case x of 
          BoolVal(true) => evalExp(e1,env)
          | BoolVal(false)=> evalExp(e2,env)
          |   _  => raise brokenTypes  
      end


and


replaceFree(e:exp,envb : bound_environment,env : environment) : exp = 
    case e of
       NumExp i            => NumExp i 
      | ConstExp s         => ConstExp s 
      | VarExp x            => if(nenvLookup(x,envb)) then rep(envLookup(x,env)) else VarExp(x)        
      | BinExp (b, e1, e2)  => BinExp (b,replaceFree(e1,envb,env),replaceFree(e2,envb,env))
      | UnaryExp(b,e) =>  UnaryExp(b,replaceFree(e,envb,env))
      | Ite(cond,e1,e2) => Ite(replaceFree(cond,envb,env),replaceFree(e1,envb,env),replaceFree(e2,envb,env))
      | Fn(id,typ1,typ2,e) => Fn(id,typ1,typ2,replaceFree(e,b_envAdd(id,envb),env))
      | AppExp(f,e) =>    AppExp(replaceFree(f,envb,env),replaceFree(e,envb,env) )          
      | LetExp(ValDecl(x,e1), e2)  =>
          let
            val v1 = replaceFree(e1,envb,env)
          in
            LetExp(ValDecl(x,v1),replaceFree(e2,b_envAdd(x,envb),env) )
          end
       | LetExp(FunDecl(x,e1),e2) =>
          let
            val v1 = replaceFree(e1,envb,env)
          in
            LetExp(ValDecl(x,v1),replaceFree(e2,b_envAdd(x,envb),env) )
        end 
and

rep(var_value : value) : exp = 
  case var_value of 
    IntVal(x) => NumExp x
    |BoolVal(x) => ConstExp x
    |FnVal(id,typ1,typ2,e) => Fn(id,typ1,typ2,e)

   

(*fun evalProgram(p : program) = 
  case p of
     Statement(u) => 
       ( case u of 
          Expression(s)=> evalExp(s,[]) 
       | Declaration(FunDecl(id,exp)) => NULL )
     | EMPTY => NULL*)

fun augment(env,fname,Fn(id,typ1,typ2,e)):value = FnVal(id,typ1,typ2,replaceFree(e,[fname,id],env))

fun evalProgram(p : program) = 
  let

    fun iter(p : program,env: environment,ls) =      
      case p of EMPTY => [NULL]
         | Statement(Expression (s)) => ( ls @ [evalExp(s,env)] )
         | Statement(Declaration(FunDecl(id,exp)))=> ( ls @ [  augment(env,id,exp) ] )
         | Program (Declaration(FunDecl(id,exp)),pr) => ( iter( pr, envAdd(id,augment(env,id,exp),env) , ls @ [ augment(env,id,exp) ] ) )
         | Program (Expression (s),pr) =>(  iter(pr,env,ls @ [evalExp(s,env)]) )
  in 
    iter(p,[],[])
  end

end;









