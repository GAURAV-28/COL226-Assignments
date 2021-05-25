structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

(*////////////////////////////////////////////////////////////////////////////////////*)
fun convertTyp(inb):typval = 
  case inb of
   Typ t        => if t = INT then IntTyp else BoolTyp
 | ARROW (a,b)  => Arrow(convertTyp(a),convertTyp(b)) 

fun convertFunTyp(t1:typval):typval =
  case t1 of
    FunTyp (_,a1,a2,_,_)  => Arrow(convertFunTyp(a1),convertFunTyp(a2))
  | Arrow x               => Arrow x
  | IntTyp                => IntTyp
  | BoolTyp               => BoolTyp

fun printFunTyp(t1:typval):string =
  case t1 of
    Arrow(x1,x2)          => printFunTyp(x1)^"->"^printFunTyp(x2)
  | IntTyp                => "int"
  | BoolTyp               => "bool"

fun printForm(for:formula):string =
  case for of
    Exp e => printExp(e) 
  | Fun (id1,id2,a1,a2,exp) => "fun "^id1^" ( "^id2^" : "^printFunTyp(convertTyp(a1))^" ) : "^printFunTyp(convertTyp(a2))^" => "^printExp(exp)
  
and
printExp(e:exp):string =
  case e of
	      NumExp i                        => Int.toString(i)
      | BoolExp s                       => if s = true then "TRUE" else "FALSE"
      | VarExp x                        => x
      | BinExp (b, e1, e2)              => printBinExp(b,e1,e2)
      | UniExp (u,e1)                   => printUniExp(u, e1)
      | IteExp (e1,e2,e3)               => "if "^printExp(e1)^" then "^printExp(e2)^" else "^printExp(e3)^" fi"
      | LetExp(ValDecl(x, e1), e2)      => "let "^x^" = "^printExp(e1)^" in "^printExp(e2)^" end"
      | LetExp(FunDecl(id1, id2, a1, a2, ex), exp)      => "let fun "^id1^" ( "^id2^" : "^printFunTyp(convertTyp(a1))^" ) : "^printFunTyp(convertTyp(a2))^" => "^printExp(ex)^" in "^printExp(exp)^" end"
      | AppExp(e1,e2)                   => "( "^printExp(e1)^" "^printExp(e2)^" )"
      | Fn (id,a1,a2,ex)                =>   "fn"^" ( "^id^" : "^printFunTyp(convertTyp(a1))^" ) : "^printFunTyp(convertTyp(a2))^" => "^printExp(ex)
  
and
printBinExp(b:binop, e1:exp, e2:exp) =
case b of
    Add   => printExp(e1)^" PLUS "^printExp(e2)
  | Sub   => printExp(e1)^" MINUS "^printExp(e2)
  | Mul   => printExp(e1)^" TIMES "^printExp(e2)
  | And   => printExp(e1)^" AND "^printExp(e2)
  | Or   => printExp(e1)^" OR "^printExp(e2)
  | Xor   => printExp(e1)^" XOR "^printExp(e2)
  | Implies   => printExp(e1)^" IMPLIES "^printExp(e2)

  | Equal   => printExp(e1)^" EQUALS "^printExp(e2)
  | Great   => printExp(e1)^" GREATERTHAN "^printExp(e2)
  | Small   => printExp(e1)^" LESSTHAN "^printExp(e2)
and
printUniExp(u:uni, e1:exp) =
case u of
    Not     => "NOT "^printExp(e1)
  | Negate  => "NEGATE "^printExp(e1)


(*////////////////////////////////////////////////////////////////////////////////////*)
fun checkStat(e::st : stat, env:typenv) : typval list = 
        let 
          val (qq,ww) = checkForm(e,env)
        in
          qq :: checkStat(st,ww)   
        end
  | checkStat([],_) = ([])
and
checkForm(form:formula,env:typenv) = 
    case form of
    Exp e  => let val qq = checkExp(e,env) in (qq,env) end
  | Fun (id1,id2,a1,a2,ex) => let 
                                val eee = typenvAdd (id1, Arrow(convertTyp(a1),convertTyp(a2)), env)
                                val p = checkExp(ex,typenvAdd(id2,convertTyp(a1),eee))
                              in 
                                if convertTyp(a2) = p then (Arrow(convertTyp(a1),convertTyp(a2)) , eee)
                                else (
                                    print("Typing Error: Function result type doesn't agree with function body type") ; 
                                    print("\nFun Result Type: "^printFunTyp(convertTyp(a2)));
                                    print("\nFun Body Type: "^printFunTyp(p));
                                    print("\nin \n  "^printForm(form));
                                    raise Fail "Typing Error"
                                )
                              end
and
checkExp(e:exp, env:typenv) =
    case e of
	      NumExp i                        => IntTyp
      | BoolExp s                       => BoolTyp
      | VarExp x                        => typenvLookup (x, env)
      | IteExp (e1,e2,e3)               => checkIteExp(e1,e2,e3,env)
      | UniExp (u,e1)                   => checkUniExp(u, e1, env)
      | BinExp (b, e1, e2)              => checkBinExp(b, e1, e2, env)
      | LetExp(ValDecl(x, e1), e2)      => checkLetExp(x,e1,e2,env)

      | LetExp(FunDecl(id1,id2,a1,a2,ex),exp) => let 
                                                  val eee = typenvAdd (id1, Arrow(convertTyp(a1),convertTyp(a2)), env)
                                                  val p = checkExp(ex,typenvAdd(id2,convertTyp(a1),eee))
                                                 in 
                                                  if convertTyp(a2) = p then checkExp(exp,eee)
                                                  else (
                                                  print("Typing Error: Function result type doesn't agree with function body type") ; 
                                                  print("\nFun Result Type: "^printFunTyp(convertTyp(a2)));
                                                  print("\nFun Body Type: "^printFunTyp(p));
                                                  print("\nin \n  "^printExp(e));
                                                  raise Fail "Typing Error"
                                                )
                                                end

      | AppExp(e1,e2)                   => checkAppExp(e1,e2,env)
      | Fn (id,a1,a2,ex) => let 
                              val p = checkExp(ex,typenvAdd(id,convertTyp(a1),env))
                            in 
                              if convertTyp(a2) = p then Arrow(convertTyp(a1),convertTyp(a2))
                              else (
                                    print("Typing Error: Function result type doesn't agree with function body type") ; 
                                    print("\nFun Result Type: "^printFunTyp(convertTyp(a2)));
                                    print("\nFun Body Type: "^printFunTyp(p));
                                    print("\nin \n  "^printExp(e));
                                    raise Fail "Typing Error"
                                )
                            end
and
checkAppExp(f:exp,e1:exp,env:typenv) = 
let
    val x = checkExp(f,env) 
    val vcc = checkExp(e1,env)
in
    case x of
    Arrow(a1,a2) => if vcc = a1 then a2
                    
                    else (
                          print("Typing Error: Function input type doesn't agree with argument type") ; 
                          print("\nFun Required Type: "^printFunTyp(a1));
                          print("\nFun Argument Type: "^printFunTyp(vcc));
                          print("\nin \n  ( "^printExp(f)^" "^printExp(e1)^" )");
                          raise Fail "Typing Error"
                         )                    
    | _ => 
          (
            print("Typing Error: Input argument doesn't agree, not of function type") ; 
            print("\nin \n  ( "^printExp(f)^" "^printExp(e1)^" )");
            raise Fail "Typing Error"
          )    
end
and
checkLetExp(x:id,for:exp,exx:exp,env:typenv) = 
              let
	              val v1 = checkExp(for,env)
	            in
	              checkExp(exx, typenvAdd (x, v1, env))
              end
and
checkBinExp(b:binop, e1:exp, e2:exp, env:typenv) =
case (b, checkExp(e1, env), checkExp(e2, env))  of
      (Add, IntTyp, IntTyp)       => IntTyp
  |   (Add, t1, t2)               =>  ( print("Typing Error: Operator and Operand doesn't agree for PLUS operation") ;
                                        print("\nRequired Type: int * int");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" PLUS "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Sub, IntTyp, IntTyp)       => IntTyp
  |   (Sub, t1, t2)               => (  print("Typing Error: Operator and Operand doesn't agree for MINUS operation") ;
                                        print("\nRequired Type: int * int");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" MINUS "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Mul, IntTyp, IntTyp)       => IntTyp
  |   (Mul, t1, t2)               => (  print("Typing Error: Operator and Operand doesn't agree for TIMES operation") ;
                                        print("\nRequired Type: int * int");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" TIMES "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (And, BoolTyp, BoolTyp)     => BoolTyp
  |   (And, t1, t2)               => (  print("Typing Error: Operator and Operand doesn't agree for AND operation") ;
                                        print("\nRequired Type: bool * bool");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" AND "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Or, BoolTyp, BoolTyp)      => BoolTyp
  |   (Or, t1, t2)                => (  print("Typing Error: Operator and Operand doesn't agree for OR operation") ;
                                        print("\nRequired Type: bool * bool");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" OR "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Xor, BoolTyp, BoolTyp)     => BoolTyp
  |   (Xor, t1, t2)               => (  print("Typing Error: Operator and Operand doesn't agree for XOR operation") ;
                                        print("\nRequired Type: bool * bool");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" XOR "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Implies, BoolTyp, BoolTyp) => BoolTyp
  |   (Implies, t1, t2)           => (  print("Typing Error: Operator and Operand doesn't agree for IMPLIES operation") ;
                                        print("\nRequired Type: bool * bool");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" IMPLIES "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Equal, IntTyp, IntTyp)     => BoolTyp
  |   (Equal, BoolTyp, BoolTyp)   => BoolTyp
  |   (Equal, t1, t2)             => (  print("Typing Error: Operator and Operand doesn't agree for EQUALS operation") ;
                                        print("\nRequired Type: bool * bool");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" EQUALS "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Great, IntTyp, IntTyp)     => BoolTyp  
  |   (Great, t1, t2)             =>  ( print("Typing Error: Operator and Operand doesn't agree for GREATERTHAN operation") ;
                                        print("\nRequired Type: bool * bool");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" GREATERTHAN "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )
  |   (Small,IntTyp, IntTyp)      => BoolTyp
  |   (Small, t1, t2)             => (  print("Typing Error: Operator and Operand doesn't agree for LESSTHAN operation") ;
                                        print("\nRequired Type: bool * bool");
                                        print("\nArgument Type: "^printFunTyp(t1)^" * "^printFunTyp(t2) );
                                        print("\nin \n  "^printExp(e1)^" LESSTHAN "^printExp(e2));
                                        raise Fail "Typing Error"
                                      )					    
and
checkUniExp(u:uni, e1:exp, env:typenv) =
case (u, checkExp(e1,env)) of
      (Not, BoolTyp)   => BoolTyp
    | (Not, t1)        => (  print("Typing Error: Operator and Operand doesn't agree for NOT operation") ;
                             print("\nRequired Type: bool");
                             print("\nArgument Type: "^printFunTyp(t1));
                             print("\nin \n  NOT "^printExp(e1));
                             raise Fail "Typing Error"
                          )
    | (Negate, IntTyp) => IntTyp
    | (Negate,t1)      => (  print("Typing Error: Operator and Operand doesn't agree for NEGATE operation") ;
                             print("\nRequired Type: int");
                             print("\nArgument Type: "^printFunTyp(t1));
                             print("\nin \n  NEGATE "^printExp(e1));
                             raise Fail "Typing Error"
                          )
and
checkIteExp(e1:exp , e2:exp, e3:exp, env:typenv) =
case checkExp(e1,env) of
      BoolTyp   => if checkExp(e2,env)=checkExp(e3,env) then checkExp(e2,env)
                   else ( print("Typing Error: Branch Types doesn't agree for if-then-else-fi operation") ;
                          print("\nIf Branch Type: "^printFunTyp(checkExp(e2,env)));
                          print("\nElse Branch Type: "^printFunTyp(checkExp(e3,env)));
                          print("\nin \n  "^"if "^printExp(e1)^" then "^printExp(e2)^" else "^printExp(e3)^" fi");
                          raise Fail "Typing Error"
                        )            
     | _        => (  print("Typing Error: If expression type doesn't agree for if-then-else-fi operation") ;
                      print("\nRequired Type: bool");
                      print("\nExpression Type: "^printFunTyp(checkExp(e1,env)));
                      print("\nin \n  "^"if "^printExp(e1)^" then "^printExp(e2)^" else "^printExp(e3)^" fi");
                      raise Fail "Typing Error"
                   )  














(*/////////////////////////////////////////////////////////////////////*)

(*fun evalStat(e::st : stat, env:environment) : value list = 
        let 
            val (qq,ww) = evalForm(e,env)
        in
            case qq of
            FunVal(_,_,_,_,_) => evalStat(st,ww) 
            | _               => qq :: evalStat(st,ww)  
        end
  | evalStat([],_) = []
and*)
fun evalStat(e::st : stat, env:environment) : value list = 
        let 
          val (qq,ww) = evalForm(e,env)
        in
          qq :: evalStat(st,ww)   
        end
  | evalStat([],_) = ([])
and
evalForm(form:formula,env:environment) = 
    case form of
    Exp e  => let val (qq,_) = evalExp(e,env) in (qq,env) end                       
  | Fun (id1,id2,a1,a2,ex) => (FunVal (id1,id2,a1,a2,ex,env),envAdd(id1,FunVal (id1,id2,a1,a2,ex,env), env)) 
and
evalExp(e:exp, env:environment) =
    case e of
	      NumExp i                                => (IntVal i,env)
      | BoolExp s                             => (BoolVal s,env)
      | VarExp x                              => (envLookup (x, env),env)
      | IteExp (e1,e2,e3)                     => evalIteExp(e1,e2,e3,env)
      | UniExp (u,e1)                         => evalUniExp(u, e1, env)
      | BinExp (b, e1, e2)                    => evalBinExp(b, e1, e2, env)
      | LetExp(ValDecl(x, e1), e2)            => evalLetExp(x,e1,e2,env)
      | LetExp(FunDecl(id1,id2,a1,a2,ex),exp) => evalExp(exp,envAdd(id1,FunVal(id1,id2,a1,a2,ex,env), env)) 
      | AppExp(e1,e2)                         => evalAppExp(e1,e2,env)
      | Fn (id,a1,a2,ex)                      => (FnVal (id,a1,a2,ex,env),env)
and
evalAppExp(e:exp,e1:exp,env:environment) = 
let
    val (x,dd) = evalExp(e,env) 
    val (vcc,kk) = evalExp(e1,dd)
in
    case x of
    FnVal (yy,a1,a2,ex,fenv)   => evalExp(ex, envAdd(yy,vcc,fenv)) 
  | FunVal (id1,yy,a1,a2,ex,fenv)  => evalExp(ex, envAdd(yy,vcc,envAdd(id1,x,fenv)))                                         
  | _ => raise brokenTypes
end
and
evalLetExp(x:id,for:exp,exx:exp,env:environment) = 
    let
	    val (v1,ww) = evalExp(for,env)
	  in
	    evalExp(exx, envAdd (x, v1, ww))
    end	
  
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment) =
case (b, evalExp(e1, env), evalExp(e2, env))  of
      (Add, (IntVal i1,_), (IntVal i2,_))       => (IntVal (i1+i2),env)
  |   (Sub, (IntVal i1,_), (IntVal i2,_))       => (IntVal (i1-i2),env)
  |   (Mul, (IntVal i1,_), (IntVal i2,_))       => (IntVal (i1*i2),env)
  |   (And, (BoolVal i1,_), (BoolVal i2,_))     => (BoolVal (i1 andalso i2),env)
  |   (Or, (BoolVal i1,_), (BoolVal i2,_))      => (BoolVal (i1 orelse i2),env)
  |   (Xor, (BoolVal i1,_), (BoolVal i2,_))     => (BoolVal ( ((not i1) andalso i2) orelse ((not i2) andalso i1) ) , env)
  |   (Implies, (BoolVal i1,_), (BoolVal i2,_)) => (BoolVal ((not i1) orelse i2) , env)
  |   (Equal, (IntVal i1,_), (IntVal i2,_))     => (BoolVal (i1 = i2) , env)
  |   (Equal, (BoolVal b1,_), (BoolVal b2,_))   => (BoolVal (b1 = b2) , env)
  |   (Great, (IntVal i1,_), (IntVal i2,_))     => (BoolVal (i1 > i2) , env)
  |   (Small, (IntVal i1,_), (IntVal i2,_))     => (BoolVal (i1 < i2) , env)
  |   _  => raise brokenTypes  					    
and
evalUniExp(u:uni, e1:exp, env:environment) =
case (u, evalExp(e1,env)) of
      (Not, (BoolVal bb,_))   => (BoolVal (not bb) , env)
    | (Negate, (IntVal ff,_)) => (IntVal (0-ff) , env )
    | _                       => raise brokenTypes
and
evalIteExp(e1:exp , e2:exp, e3:exp, env:environment) =
case (evalExp(e1,env)) of
        (BoolVal b,_) => if b then evalExp(e2,env) else evalExp(e3,env)
    |   _ => raise Fail "Invalid if-then-else-fi statement!"

end