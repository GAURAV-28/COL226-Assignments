structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  exception badCharacter
  val pos = ref 1
  val c = ref 1
  val out = ref "["
  val eof = fn () => (Tokens.EOF(!pos, !pos))
  val error = fn (e, l:int, c:int) => TextIO.output(TextIO.stdOut,"Unknown Token:"^Int.toString(l)^":"^Int.toString(c)^":"^e)
  val giveCol = fn () => !c
  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))

%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z0-9];
digit=[0-9];
ws = [\ \t\n];

%%

";"      => (c := !c + 1 ; out := !out^"TERM \";\","; Tokens.TERM(!pos,!pos));
":"      => (c := !c + 1 ; out := !out^"COLON \":\","; Tokens.COLON(!pos,!pos));
"->"     => (c := !c + 2 ; out := !out^"ARROW \"->\","; Tokens.ARROW(!pos,!pos));
"=>"     => (c := !c + 2 ; out := !out^"DARROW \"=>\","; Tokens.DARROW(!pos,!pos));
"TRUE"   => (c := !c + 4 ; out := !out^"CONST \"TRUE\","; Tokens.CONST(yytext,!pos,!pos));
"FALSE"  => (c := !c + 5 ; out := !out^"CONST \"FALSE\","; Tokens.CONST(yytext,!pos,!pos));
"AND"    => (c := !c + 3 ; out := !out^"AND \"AND\","; Tokens.AND(!pos,!pos));
"OR"     => (c := !c + 2 ; out := !out^"OR \"OR\","; Tokens.OR(!pos,!pos));
"XOR"    => (c := !c + 3 ; out := !out^"XOR \"XOR\","; Tokens.XOR(!pos,!pos));
"NOT"    => (c := !c + 3 ; out := !out^"NOT \"NOT\","; Tokens.NOT(!pos,!pos));
"EQUALS" => (c := !c + 6 ; out := !out^"EQUALS \"EQUALS\","; Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (c := !c + 7 ;out := !out^"IMPLIES \"IMPLIES\","; Tokens.IMPLIES(!pos,!pos));
"fn"     => (c := !c + 2 ; out := !out^"fn \"fn\","; Tokens.FN(!pos,!pos));
"fun"    => (c := !c + 3 ; out := !out^"fun \"fun\","; Tokens.FUN(!pos,!pos));
"int"    => (c := !c + 3 ; out := !out^"int \"int\","; Tokens.INT(!pos,!pos));
"bool"   => (c := !c + 4 ; out := !out^"bool \"bool\","; Tokens.BOOL(!pos,!pos));
"if"     => (c := !c + 2 ; out := !out^"if \"if\","; Tokens.IF(!pos,!pos));
"then"   => (c := !c + 4 ; out := !out^"then \"then\","; Tokens.THEN(!pos,!pos));
"else"   => (c := !c + 4 ; out := !out^"else \"else\","; Tokens.ELSE(!pos,!pos));
"fi"     => (c := !c + 2 ; out := !out^"fi \"fi\","; Tokens.FI(!pos,!pos));
"in"     => (c := !c + 2 ; out := !out^"in \"in\","; Tokens.IN(!pos,!pos));
"end"    => (c := !c + 3 ; out := !out^"end \"end\","; Tokens.END(!pos,!pos));
"let"    => (c := !c + 3 ; out := !out^"let \"let\","; Tokens.LET(!pos,!pos));
"("      => (c := !c + 1 ; out := !out^"LPAREN \"(\","; Tokens.LPAREN(!pos,!pos));
")"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.RPAREN(!pos,!pos));
"="      => (c := !c + 1 ; out := !out^"EQ \"=\","; Tokens.EQ(!pos,!pos));
"PLUS"   => (c := !c + 4 ; out := !out^"PLUS \"PLUS\","; Tokens.PLUS(!pos,!pos));
"MINUS"   => (c := !c + 5 ; out := !out^"MINUS \"MINUS\","; Tokens.MINUS(!pos,!pos));
"TIMES"   => (c := !c + 5 ; out := !out^"TIMES \"TIMES\","; Tokens.TIMES(!pos,!pos));
"NEGATE"   => (c := !c + 6 ; out := !out^"NEGATE \"NEGATE\","; Tokens.NEGATE(!pos,!pos));
"GREATERTHAN"   => (c := !c + 11 ; out := !out^"GREATERTHAN \"GREATERTHAN\","; Tokens.GREATERTHAN(!pos,!pos));
"LESSTHAN"   => (c := !c + 11 ; out := !out^"LESSTHAN \"LESSTHAN\","; Tokens.LESSTHAN(!pos,!pos));
{digit}+ => (c := !c + size(yytext) ; out := !out^"NUM \""^yytext^"\","; Tokens.NUM (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));
{alpha}+ => (c := !c + size(yytext) ; out := !out^"ID \""^yytext^"\","; Tokens.ID(yytext,!pos,!pos));
{ws}    => (c := !c + 1 ; lex());
.        => (error(yytext,!pos,!c); out := "["; c := 1; pos := 1; raise badCharacter; lex());

