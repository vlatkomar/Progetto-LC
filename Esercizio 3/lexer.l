%option noyywrap
%{
#include "parser.tab.h"
#include <stdlib.h>
%}

label [A-Za-z][A-Za-z0-9]*
inizio_sez "{"[[:blank:]\n]*
fine_sez [[:blank:]\n]*"}"j
cifre [0-9]+
stringa ([[:print:][:blank:]]{-}[{}])*
booleano [true|false]

%%

{label}[\t]*{inizio_sez}     {return INIZIO;}
"#"{stringa}                 {return COMMENTO;}
{label}			     {return VARIABILE;}
{fine_sez}                   {return FINE;}
{cifre}		             {return NUMERO;}
{stringa}		     {return STRINGA;} 
[ \t\n]+	             ;










 




