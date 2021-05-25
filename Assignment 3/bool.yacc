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
| PLUS | MINUS | TIMES | GREATERTHAN | LESSTHAN | NEGATE
| ARROW | DARROW | INT | BOOL | FN | FUN | COLON
| IF | THEN | ELSE | FI | RPAREN | LPAREN | EOF

%nonterm EXP of AST.exp | START of AST.stat | DECL of AST.decl | statement of AST.stat | TYP of AST.arrow | formula of AST.formula | FTYP of AST.arrow

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

%right ARROW
%nonassoc DARROW COLON 
%right FN
%left EQUALS GREATERTHAN LESSTHAN
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
  statement: formula (AST.formulaAdd(formula,[])) 
           | formula TERM statement (AST.formulaAdd(formula,statement))

  formula: EXP (AST.Exp(EXP))
         | FUN ID LPAREN ID COLON FTYP RPAREN COLON FTYP DARROW EXP (AST.Fun(ID1,ID2,FTYP1,FTYP2,EXP))

      
  DECL: ID EQ EXP (AST.ValDecl(ID,EXP))
      | FUN ID LPAREN ID COLON FTYP RPAREN COLON FTYP DARROW EXP (AST.FunDecl(ID1,ID2,FTYP1,FTYP2,EXP))

  TYP: INT (AST.Typ(AST.INT))
     | BOOL (AST.Typ(AST.BOOL))

  FTYP: FTYP ARROW FTYP (AST.ARROW(FTYP1,FTYP2))
      | TYP (TYP) 
      | LPAREN FTYP RPAREN (FTYP)


  EXP: NUM (AST.NumExp(NUM))
  | ID (AST.VarExp(ID))
  | CONST (AST.BoolExp(value CONST))

  | EXP PLUS EXP (AST.BinExp(AST.Add, EXP1,  EXP2))
  | EXP MINUS  EXP (AST.BinExp(AST.Sub,  EXP1,  EXP2))
  | EXP TIMES  EXP (AST.BinExp(AST.Mul,  EXP1, EXP2))

  | LET DECL IN EXP END (AST.LetExp(DECL, EXP))
  | LPAREN EXP EXP RPAREN (AST.AppExp(EXP1,EXP2))
  | FN LPAREN ID COLON FTYP RPAREN COLON FTYP DARROW EXP (AST.Fn(ID,FTYP1,FTYP2,EXP))
  
  | EXP EQUALS EXP (AST.BinExp(AST.Equal, EXP1, EXP2))
  | EXP GREATERTHAN EXP (AST.BinExp(AST.Great, EXP1, EXP2))
  | EXP LESSTHAN EXP (AST.BinExp(AST.Small, EXP1, EXP2))

  | EXP AND EXP (AST.BinExp(AST.And, EXP1, EXP2))
  | EXP OR EXP (AST.BinExp(AST.Or, EXP1, EXP2))
  | EXP XOR EXP (AST.BinExp(AST.Xor, EXP1, EXP2))
  | EXP IMPLIES EXP (AST.BinExp(AST.Implies, EXP1, EXP2))

  | LPAREN EXP RPAREN (EXP)
  | IF EXP THEN EXP ELSE EXP FI (AST.IteExp(EXP1, EXP2, EXP3))

  | NOT EXP (AST.UniExp(AST.Not, EXP))
  | NEGATE EXP (AST.UniExp(AST.Negate, EXP))






