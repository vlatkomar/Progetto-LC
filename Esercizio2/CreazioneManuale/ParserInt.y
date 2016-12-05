{
module Main where
import Abstract
import Lexer
}

%name parserint
%tokentype { Token }
%error { parseError }

%token
   " "		{ Space }
   "("		{ Open }
   ")"		{ Close }
   int		{ Int $$}

%%

-- regole di grammatica

Tree : 		     { Void } |
	Int " " Sons { Node $1 $3 } |
	Int	     { Node $1 [Void] }

Sons :  Son	     { [$1] } |
	Son " " Sons { $1:$3 }

Son :   Int	     { Node $1 [Void] } |
	"(" Tree ")" { $2 }

Int:    int          { $1 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
