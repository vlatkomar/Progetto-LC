{
module ParserFloat where
import Abstract
import Lexer
}

%name parserfloat
%tokentype { Token }
%error { parseError }

%token
   " "		{ Space }
   "("		{ Open }
   ")"		{ Close }
   float	{ Float $$}

%%

-- regole di grammatica

Tree : 		       { Void } |
	Float " " Sons { Node $1 $3 } |
	Float 	       { Node $1 [Void] }

Sons :  Son	       { [$1] } |
	Son " " Sons   { $1:$3 }

Son :   Float 	       { Node $1 [Void] } |
	"(" Tree ")"   { $2}

Float:  float	       { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
