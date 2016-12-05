%{
#define YYSTYPE char*
#include <stdio.h> 
#include <string.h>
#include <stdlib.h> 

//creo una struttura dati per contenere le variabili trovate nella sezione che punta a questa lista
struct listaVariabili {
	char* nomeVariabile;			
	char* valore;		
	struct listaVariabili* next;		
	char* tipo;	//tipo della variabile
};

//creo una struttura dati per contenere le varie sezioni trovate dal lexer
struct listaSezioni {                         
	char* nomeSezione;           
	struct listaSezioni* next;  
	struct listaVariabili* primoNodo; // puntatore alla lista delle variabili di sezione
};

//creo una struttura dati per contenere i commenti associati alle rispettive sezioni
struct listaCommento {
	char* testo;        //contiene il commento trovato
  	char* sezione;      //in questa sezione	
  	struct commento* next;
};

struct listaSezione* testaSezione = NULL;
struct listaVariabili* testaVariabile = NULL;
struct listaCommento* testaCommento = NULL;

char* tipoVariabili;     //contiene il tipo delle variabili
char* sezioneCorrente;   //contiene il nome della sezione che si sta leggendo(serve per salvare i commenti)

%}

%token INIZIO COMMENTO VARIABILE STRINGA NUMERO BOOLEANO FINE
%%

file:	%empty
	|	sezione file
;

sezione: INIZIO scope 
               {
			struct listaSezioni* pt = malloc (sizeof (struct listaSezioni));  //inizializzo listaSezioni
			pt->nomeSezione = $1;
			sezioneCorrente = $1;              
			pt->primoNodo = testaVariabile;       // faccio puntare primoNodo alla lista delle variabili
			pt->next=NULL;                  
			testaVariabile = NULL; // lo inizializzo                
			if (testaSezione == NULL) // se la lista è vuota inserisco qui la sezione attuale
                                testaSezione = pt;
			else{         
		                struct listaSezioni* current = testaSezione; // copio il riferimento alla testa
				while ((current->next)!=NULL){
                   			//controllo se ci sono sezioni ripetute, fino al penultimo elemento
					if(strcmp(current->nomeSezione, pt->nomeSezione)==0){//se si, termino con errore
						fprintf (stderr, "ERRORE: è stata definita due volte la sezione: %s", current->nomeSezione);
						exit(1); //termino con errore
					} 
					current=current->next; //mi sposto al prossimo elemento
				}
			(current->next)=pt; //manca da controllare l'ultimo elemento
			if(strcmp(current->nomeSezione, pt->nomeSezione)==0){
				fprintf (stderr, "ERRORE: è stata definita due volte la sezione: %s", current->nomeSezione);
				exit(1);
				}
			}
		}
;

//scope e'tutto cio' che sta all'interno di una sezione	
scope: assegnamento scope 
       | commento scope 
       | FINE {$$=$1;}
;


assegnamento:	etichetta '=' valore 	{ 

                //inizializzo listaVariabili
		struct listaVariabili* pt = malloc (sizeof (struct listaVariabili));
		pt->nomeVariabile = $1;
		pt->tipo = tipo;
		pt->variabile = $3; // valore della variabile
		pt->next=NULL;      // creo puntatore vuoto
		if (testaVariabile == NULL) testaVariabile=pt; // se la testa è vuota inserisco qui
		else{
			struct listaVariabili* current = testaVariabile;
			//adesso scandisco la lista a partire da current	
			while ((current->next)!=NULL){
                                //controllo se ci sono variabili definite piu' volte in questa sezione, fino al penultimo elemento
				if(strcmp(current->nomeVariabile, pt->nomeVariabile)==0)
                                        fprintf (stderr, "WARNING: è stata ridefinita la variabile %s \n", current->nomeVariabile);
				current=current->next; //mi sposto al prossimo elemento
			}
			(current->next)=pt; //manca da controllare l'ultimo elemento
			if(strcmp(current->nomeVariabile, pt->nomeVariabile)==0) 
                        	fprintf (stderr, "warning: clash di nome di variabili %s \n", current->nomeVariabile);
		}
	}
;

etichetta: VARIABILE  {$$=$1;}
;

commento:  COMMENTO 
	{
		//inizializzo listaCommento
		struct listaCommento* pt = malloc (sizeof (struct listaCommento));
		pt->testo = $1;
		pt->sezione = sezioneCorrente;
		if (testaCommento == NULL) testaCommento=pt; // se la testa è vuota inserisco qui
		else{
			struct listaCommento* current = testaCommento; printf("comm");
		}
	}
;

//associo un carattere a ogni tipo di variabile	
valore:	STRINGA 	{ tipoVariabile = 's'; $$=$1; }
	| NUMERO	{ tipoVariabile = 'n'; $$=$1; }
	| BOOLEANO	{ tipoVariabile = 'b'; $$=$1; }
;

%%

int main () {
	yyparse(); 
}


//generico messsaggio d'errore
int yyerror (char *msg) {
	return fprintf (stderr, "YACC: %s\n", msg);
}

