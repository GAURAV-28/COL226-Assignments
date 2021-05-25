structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  exception badCharacter
  val pos = ref 1
  val c = ref 1
  val out = ref "["
  val eof = fn () => (if size(!out) = 1 then print(!out^"]\n") else print(String.substring(!out,0,size(!out)-1)^"]\n"); out := "["; c := 1; pos := 1; Tokens.EOF(!pos, !pos))
  val error = fn (e, l:int, c:int) => TextIO.output(TextIO.stdOut,"Unknown Token:"^Int.toString(l)^":"^Int.toString(c)^":"^e)
  val giveCol = fn () => !c
  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t\n];

%%
";"      => (c := 1 ; pos := (!pos) + 1; out := !out^"TERM \";\","; Tokens.TERM(!pos,!pos));
{ws}    => (c := !c + 1 ; lex());
"TRUE"   => (c := !c + 4 ; out := !out^"CONST \"TRUE\","; Tokens.CONST(yytext,!pos,!pos));
"FALSE"  => (c := !c + 5 ; out := !out^"CONST \"FALSE\","; Tokens.CONST(yytext,!pos,!pos));
"AND"    => (c := !c + 3 ; out := !out^"AND \"AND\","; Tokens.AND(!pos,!pos));
"OR"     => (c := !c + 2 ; out := !out^"OR \"OR\","; Tokens.OR(!pos,!pos));
"XOR"    => (c := !c + 3 ; out := !out^"XOR \"XOR\","; Tokens.XOR(!pos,!pos));
"NOT"    => (c := !c + 3 ; out := !out^"NOT \"NOT\","; Tokens.NOT(!pos,!pos));
"EQUALS" => (c := !c + 6 ; out := !out^"EQUALS \"EQUALS\","; Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (c := !c + 7 ;out := !out^"IMPLIES \"IMPLIES\","; Tokens.IMPLIES(!pos,!pos));
"IF"     => (c := !c + 2 ; out := !out^"IF \"IF\","; Tokens.IF(!pos,!pos));
"THEN"   => (c := !c + 4 ; out := !out^"THEN \"THEN\","; Tokens.THEN(!pos,!pos));
"ELSE"   => (c := !c + 4 ; out := !out^"ELSE \"ELSE\","; Tokens.ELSE(!pos,!pos));
"("      => (c := !c + 1 ; out := !out^"LPAREN \"(\","; Tokens.LPAREN(!pos,!pos));
")"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.RPAREN(!pos,!pos));
{alpha}+ => (c := !c + size(yytext) ; out := !out^"ID \""^yytext^"\","; Tokens.ID(yytext,!pos,!pos));
.        => (error(yytext,!pos,!c); out := "["; c := 1; pos := 1; raise badCharacter; lex());

