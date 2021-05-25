structure AST =
struct
type id = string
exception Error
datatype binop = Add | Sub | Mul | Equal | Great | Small | And | Or | Xor | Implies 
datatype paren = Lparen | Rparenn
datatype uni = Negate | Not

datatype arrow = ARROW of arrow*arrow | Typ of typ
and typ = INT | BOOL

datatype decl = ValDecl of id * exp | FunDecl of id * id * arrow * arrow * exp
and formula = Exp of exp
            | Fun of id * id * arrow * arrow * exp


and exp = NumExp of int
    	| VarExp of id
      | Fn of id * arrow * arrow * exp
		| BoolExp of bool
		| BinExp of binop * exp * exp
		| LetExp of decl * exp
		| IteExp of exp * exp * exp
		| UniExp of uni * exp
        | AppExp of exp * exp
				       
datatype value = IntVal of int
	       	     | BoolVal of bool
               | FunVal of id * id * arrow * arrow * exp * ((id*value) list)
               | FnVal of id * arrow * arrow * exp * ((id*value) list)
               
type stat = (formula) list
				
type environment = (id * value) list

fun formulaAdd (e:formula , statement:stat) = e::statement

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envAddAll ([] , env2:environment) = env2
  | envAddAll (x::env : environment , env2:environment) = envAddAll(env, x::env2)  

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       	SOME (x, v)   => v
				    |   NONE => raise Fail ("Error: unbound variable: "^var)
							
fun envRLookup (var:value, env:environment) =
    case List.find(fn (_, x) => x = var) env of
				       	SOME (v, x)   => v
				    |   NONE => ""

fun envDelete (var:id, []) = []
  | envDelete (var:id, y::ys) = let val (s,_) = y in if s = var then envDelete(var,ys) else y::envDelete(var,ys) end

(*////////////////////////////////////////////////////////////////////////////////////////*)

datatype typval = IntTyp 
	       	      | BoolTyp
                | Arrow of typval*typval
                | FunTyp of id*typval*typval*formula*((id*typval) list)


type typenv = (id * typval) list

fun typenvAdd (var:id, v:typval, tenv:typenv) =
    (var,v)::tenv

fun typenvLookup (var:id, tenv:typenv) =
    case List.find(fn (x, _) => x = var) tenv of
				       	SOME (x, v)   => v
				    |   NONE => raise Fail ("Error: unbound variable: "^var)

fun typenvRLookup (var:typval, env:typenv) =
    case List.find(fn (_, x) => x = var) env of
				       	SOME (v, x)   => v
				    |   NONE => ""
end





(*  
formula: NOT formula ("NOT NOT,"^formula^"formula => NOT formula,")
       | formula OR formula (formula1^"OR OR,"^formula2^"formula => formula OR formula,")
       | formula AND formula (formula1^"AND AND,"^formula2^"formula => formula AND formula,")
       | formula XOR formula (formula1^"XOR XOR,"^formula2^"formula => formula XOR formula,")
       | formula IMPLIES formula (formula1^"IMPLIES IMPLIES,"^formula2^"formula => formula IMPLIES formula,")
       | formula EQUALS formula (formula1^"EQUALS EQUALS,"^formula2^"formula => formula EQUALS formula,")
       | IF formula THEN formula ELSE formula ("IF IF,"^formula1^"THEN THEN,"^formula2^"ELSE ELSE,"^formula3^"formula => IF formula THEN formula ELSE formula,")
       | LPAREN formula RPAREN ("LPAREN (,"^formula^"RPAREN ),formula => (formula),")
       | CONST (value CONST)
       | ID (lookup ID)
*)

(*
(* User  declarations *)
fun lookup s = "ID "^s^",formula => ID,"
fun value "TRUE" = true
|   value "FALSE" = false


%%
(* required declarations *)
%name Bool

%term
  ID of string | CONST of string | NUM of int | EQ | TERM
| NOT | AND | OR | XOR | EQUALS | IMPLIES | LET | IN | END 
| PLUS | MINUS | TIMES | GREATERTHAN | SMALLERTHAN | NEGATE
| ARROW | DARROW | INT | BOOL | FN | FUN | COLON
| IF | THEN | ELSE | FI | RPAREN | LPAREN | EOF

%nonterm EXP of AST.exp | START of AST.stat | DECL of AST.decl | statement of AST.stat

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

%left EQUALS GREATERTHAN SMALLERTHAN
%left PLUS MINUS
%left TIMES
%right NEGATE
%right IF THEN ELSE 
%nonassoc FI 
%right IMPLIES 
%left XOR OR AND
%right NOT

%start START

%verbose

%%

  START: statement (statement)
  statement: EXP (AST.expAdd(EXP,[])) 
           | EXP TERM statement (AST.expAdd(EXP,statement))

  DECL: ID EQ EXP (AST.ValDecl(ID, EXP))
  

  EXP: NUM (AST.NumExp(NUM))
  | ID (AST.VarExp(ID))
  | CONST (AST.BoolExp(value CONST))

  | EXP PLUS EXP (AST.BinExp(AST.Add, EXP1,  EXP2))
  | EXP MINUS  EXP (AST.BinExp(AST.Sub,  EXP1,  EXP2))
  | EXP TIMES  EXP (AST.BinExp(AST.Mul,  EXP1, EXP2))

  | LET DECL IN EXP END (AST.LetExp(DECL, EXP))

  | EXP EQUALS EXP (AST.BinExp(AST.Equal, EXP1, EXP2))
  | EXP GREATERTHAN EXP (AST.BinExp(AST.Great, EXP1, EXP2))
  | EXP SMALLERTHAN EXP (AST.BinExp(AST.Small, EXP1, EXP2))

  | EXP AND EXP (AST.BinExp(AST.And, EXP1, EXP2))
  | EXP OR EXP (AST.BinExp(AST.Or, EXP1, EXP2))
  | EXP XOR EXP (AST.BinExp(AST.Xor, EXP1, EXP2))
  | EXP IMPLIES EXP (AST.BinExp(AST.Implies, EXP1, EXP2))

  | LPAREN EXP RPAREN (EXP)
  | IF EXP THEN EXP ELSE EXP FI (AST.IteExp(EXP1, EXP2, EXP3))

  | NOT EXP (AST.UniExp(AST.Not, EXP))
  | NEGATE EXP (AST.UniExp(AST.Negate, EXP))

///////////////////////////////////
structure AST =
struct
type id = string
exception Error
datatype binop = Add | Sub | Mul | Equal | Great | Small | And | Or | Xor | Implies 
datatype paren = Lparen | Rparenn
datatype uni = Negate | Not

datatype decl = ValDecl of id * exp

and exp = NumExp of int
    	| VarExp of id
		| BoolExp of bool
		| BinExp of binop * exp * exp
		| LetExp of decl * exp
		| IteExp of exp * exp * exp
		| UniExp of uni * exp
				       
datatype value = IntVal of int
	       	   | BoolVal of bool

type stat = (exp) list
				
type environment = (id * value) list

fun expAdd (e:exp , statement:stat) = e::statement

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       	SOME (x, v)   => v
				    |   NONE => raise Fail ("Error: unbound variable: "^var)
							
end



*)