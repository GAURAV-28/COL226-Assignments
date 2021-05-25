(* User  declarations *)
fun lookup s = "ID "^s^",formula => ID,"
fun value "TRUE" = "CONST TRUE,formula => CONST,"
|   value "FALSE" = "CONST FALSE,formula => CONST,"



%%
(* required declarations *)
%name Bool

%term
  ID of string | CONST of string
| NOT | AND | OR | XOR | EQUALS | IMPLIES | TERM
| IF | THEN | ELSE | RPAREN | LPAREN | EOF

%nonterm program of string | statement of string | formula of string | START of string

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF


%right IF THEN ELSE 
%right IMPLIES 
%left EQUALS XOR OR AND
%right NOT

%start START

%verbose

%%
START: program (program^"START => program")
program: statement program (statement^"statement => formula TERM,"^program^"program => statement program,")
       | statement (statement^"statement => formula TERM,program => statement,")
statement: formula TERM (formula^"TERM ;,") 
  
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

