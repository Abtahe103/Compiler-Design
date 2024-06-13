/* C Declarations */

%{
	#include<stdio.h>
	#include<stdlib.h>
	#include <string.h>
	#include <math.h>
	#include "header.h"
	int yyparse();
	int yylex();
	int yyerror();
	extern int yylineno;
	int isdeclared(char*);
	int addnewval(char*,int );
	int getval(char*);
	int setval(char* , int );
   
%}

%union {
  char text[1000];
  int value;
}

%token <text>  ID
%token <value>  NUMBER
%token <text> STR
%type <value> expression
%type <value> secondstatement
%left LESSTHAN GREATERTHAN LESSEQUAL GREATEREQUAL
%left PLUS MINUS
%left MULTIPLY DIVIDE

%token	INT DOUBLE CHAR BEG FBS FBE SBS SBE SEMICOLON COMMA ASSIGN PRINTVAR PRINTSTR PRINTLN PRINTFUNC
%token	PLUS MINUS MULTIPLY DIVIVE LESSTHAN GREATERTHAN LESSEQUAL GREATEREQUAL MOD FACT
%token	IF ELSE ELSEIF FOR TO SWITCH DEFAULT COLON UDFUNCTION EQUAL NOTEQUAL LOGFUNC LOG10FUNC TANFUNC
%token	MAX MIN ODDEVEN SUMDIGIT REVNUM COMPAREMAX COMPAREMIN REVERSE SORT SINFUNC COSFUNC GCDFUNC LCMFUNC POWERFUNC

%nonassoc IF
%nonassoc ELSE
%nonassoc UMINUS

%%

start	 	: function program { printf("\n\n\n----------- Compilation Successful -------------\n\n"); }
			;
program		: BEG SBS statement SBE SEMICOLON
			;
statement	: /* empty */
			| statement declaration
			| statement print
			| statement expression 
			| statement ifelse
			| statement assign
			| statement forloop
			| statement switch
			| statement loopbody  
			| statement secondstatement
			;

/*--------declaration--------*/

declaration : type variables SEMICOLON 
			;
type		: INT | DOUBLE | CHAR 
			;
variables	: variable COMMA variables 
			| variable 
			;
variable   	: ID 							{//printf("%s\n",$1);
												int x = addnewval($1,0);
												if(!x) {
													printf("Compilation Error:\nLine no: %d   Variable %s is already declared\n",yylineno,$1);
													exit(-1);
												}

											}
			| ID ASSIGN expression 			{//printf("%s %d\n",$1,$3);
												int x = addnewval($1,$3);
												if(!x) {
													printf("Compilation Error:\nLine no: %d   Variable %s is already declared\n",yylineno,$1);
													exit(-1);
													}
											}
			| ID ASSIGN secondstatement 			{//printf("%s %d\n",$1,$3);
												int x = addnewval($1,$3);
												if(!x) {
													printf("Compilation Error:\nLine no: %d   Variable %s is already declared\n",yylineno,$1);
													exit(-1);
													}
											}

			;





/*------variable assign-----*/

secondstatement : MAX FBS expression FBE SEMICOLON			{ printf("%d\n",$3);}
				| MIN FBS expression FBE SEMICOLON			{ printf("%d\n",$3);}
				| SINFUNC FBS expression FBE SEMICOLON 		{ printf("%lf\n",sin( $3*3.1416/180)); }
				| COSFUNC FBS expression FBE SEMICOLON 		{ printf("%lf\n",cos( $3*3.1416/180)); }
				| TANFUNC FBS expression FBE SEMICOLON		{ printf("%lf\n",tan( $3*3.1416/180)); }
				| LOG10FUNC FBS expression FBE SEMICOLON		{ printf("%lf\n",(log( $3*1.0)/log(10.0))); }
				| LOGFUNC FBS expression FBE SEMICOLON		{ printf("%lf\n",(log( $3)));}
				| FBS expression FBE SEMICOLON				{ $$=$2;}
				| FACT FBS expression FBE SEMICOLON			{ int n=$3;
																int ans=1,i;
																for(i=n;i>1;i--)
																	ans= ans * i;
																printf("%d\n",ans);
															}
				| ODDEVEN FBS expression FBE SEMICOLON			{ int n=abs($3);
																	if (n%2==0)	{	
																printf("even\n");}
																else { printf("odd\n"); }
															}
				| SUMDIGIT FBS expression FBE SEMICOLON			{ int s=0, n=$3, m;
																	while(n>0){    
																	m=n%10;    
																	s+=m;    
																	n/=10;    
																	}    
																	printf("%d\n",s);
															}
				| REVNUM FBS expression FBE SEMICOLON			{ int n=$3, reverse=0, rem; 
															while(n!=0){    
																	rem=n%10;    
																	reverse=reverse*10+rem;    
																	n/=10;    
																}     
																	printf("%d\n",reverse);
															}
				| REVERSE FBS STR FBE SEMICOLON				{ int l = strlen($3);
																int i;
																for(i = l-2;  i >0; i--) 
																	printf("%c",$3[i]);
															}
				| SORT FBS STR FBE SEMICOLON					{ int l = strlen($3);
																int i,j;
																for(i=1; i<l-1; i++)
																{
																	for(j=i+1; j<l-1; j++)
																	{
																		if($3[i] > $3[j])
																		{
																			char temp     = $3[i];
																			$3[i] = $3[j];
																			$3[j] = temp;
																		}
																	}
																}
																for(i = 1;  i < l-1; i++) 
																	printf("%c",$3[i]);
															}
				;


assign : ID ASSIGN expression SEMICOLON 					{	if(!isdeclared($1)) {
																	printf("Compilation Error:\nLine no: %d   Variable %s is not declared\n",yylineno,$1);
																	exit(-1);
																}
																else{
																	setval($1,$3);
																}
															}
		|ID ASSIGN secondstatement SEMICOLON 					{	if(!isdeclared($1)) {
																	printf("Compilation Error:\nLine no: %d   Variable %s is not declared\n",yylineno,$1);
																	exit(-1);
																}
																else{
																	setval($1,$3);
																}
															}
		;






/*--------print----------*/

print		: PRINTVAR FBS ID FBE SEMICOLON 					{	if( isdeclared($3) ){
																	printf("%d",getval($3));
																}
																else{
																	printf("Compilation Error:\nLine no: %d   Variable %s is not declared\n",yylineno,$3);
																	exit(-1);
																}
															}
			| PRINTSTR FBS STR FBE SEMICOLON 				{	int i,l = strlen($3);
															for(i = 1;  i < l-1; i++) {
																	printf("%c",$3[i]); }
															}
			| PRINTLN FBS FBE SEMICOLON 						{	printf("\n");}
			
			| PRINTFUNC secondstatement
			;

ifbody	: /* empty */
			| ifbody ifstatement
			;


ifstatement		: PRINTVAR FBS ID FBE SEMICOLON 				{	pop++;
																if(!isdeclared($3)){
																	printf("Compilation Error:\nLine no: %d   Variable %s is not declared\n",yylineno,$3);
																	exit(-1);
																}
																else{
																	mark=1;
																	v1[pop] = getval($3);
																}							
															}
				| PRINTSTR FBS STR FBE SEMICOLON 			{	pop++;
																mark=2;
																strcpy(keep[pop],$3);	
															}
			| PRINTLN FBS FBE SEMICOLON 						{	pop++;
																mark=3;
															}
			;
			
loopbody	: /* empty */
			| loopbody loopstatement
			;
			
loopstatement		:
			 PRINTVAR FBS ID FBE SEMICOLON 					{
																if(!isdeclared($3)){
																	printf("Compilation Error:\nLine no: %d   Variable %s is not declared\n",yylineno,$3);
																	exit(-1);
																}
																else{
																	mark=1;
																	v1[3] = getval($3);
																}
															}
			| PRINTSTR FBS STR FBE SEMICOLON 				{
																mark = 2;
																strcpy(keep[3],$3);
															}
			| PRINTLN FBS FBE SEMICOLON 						{
																mark = 3;
															}
			;






/*--------expression --------*/

expression : NUMBER 							{ $$ = $1;}
			| ID 								{ if(!isdeclared($1)) {
														printf("Compilation Error:\nLine no: %d   Variable %s is not declared\n",yylineno,$1);
														exit(-1);
													}
												  else{
													$$ = getval($1);
													}
												}
			| MINUS expression %prec UMINUS		{ $$ = -$2;}
			| expression PLUS expression 		{ $$ = $1 + $3;}
			| expression MINUS expression 		{ $$ = $1 - $3;}
			| expression MULTIPLY expression 	{ $$ = $1 * $3;}
			| expression DIVIDE expression 		{ if($3) {
 													$$ = $1 / $3;
													}
				  								  else {
													$$ = 0;
													printf("\nError:\nLine no: %d   division by zero\n",yylineno);
													exit(-1);
													} 
												}
			| expression MOD expression 		{ if($3)
													$$ = $1 % $3;
												else
													$$=0;
												}
			| expression COMPAREMAX expression 	{ if($1>$3) 
													$$=$1;
												  else 
												  	$$=$3;
												}
			| expression COMPAREMIN expression { if($3>$1) 
														$$=$1;
													 else 
													 	$$=$3;
													}
			| expression GCDFUNC expression 		{ int n1=$1;
														int n2=$3;
														while(n1!=n2)
														{
															if(n1 > n2)
																n1 -= n2;
															else
																n2 -= n1;
														}
														//printf("%d\n",n1);
														$$ = n1;
													}
			| expression LCMFUNC expression 		{ int n1=$1;
														int n2=$3;
														int a=n1*n2;
														while(n1!=n2)
														{
															if(n1 > n2)
																n1 -= n2;
															else
																n2 -= n1;
														}
														n1=a/n1;
														//printf("%d\n",n1);
														$$ = n1;
													}
			| expression POWERFUNC expression 		{ int n1=$1;
														int n2=$3;
														int ans=1;
														while(n2)
														{
															ans*=n1;
															n2--;
														}
														//printf("%d\n",ans);
														$$ = ans;
													}
													
			| expression LESSTHAN expression		{ $$ = $1 < $3; }
			| expression GREATERTHAN expression		{ $$ = $1 > $3; }
			| expression LESSEQUAL expression		{ $$ = $1 <= $3; }
			| expression GREATEREQUAL expression	{ $$ = $1 >= $3; }
			| expression EQUAL expression			{ $$ = $1 == $3; }
			| expression NOTEQUAL expression		{ $$ = $1 != $3; }
			| FBS expression FBE						{$$ = $2;}
			;






/*---------ifelse ----------*/

ifelse 	: IF FBS ifexp FBE SBS ifbody SBE elseif		{	ifdone[ifptr] = 0;
															ifptr--;
														
															if(mark1){
																if(mark==1){
																	printf("%d",v1[0]);
																}
																else if(mark==2){
																	int i;
																	int l=strlen(keep[0]);
																	for(i = 1;  i < l-1; i++) 
																		printf("%c",keep[0][i]);
																}
																else{
																	printf("\n");
																}
															}
														}
		;
ifexp	: expression 								{	ifptr++;
														ifdone[ifptr] = 0;
														pop = -1;
														mark1 = 0;
														if($1){
															mark1 = 1;
															ifdone[ifptr] = 1;
														}
													}
		;
elseif 	: /* empty */
		| elseif ELSEIF FBS expression FBE SBS ifbody SBE	{	if($4 && ifdone[ifptr] == 0){
																	ifdone[ifptr] = 1;
																	if(mark==1){
																		printf("%d",v1[1]);
																	}
																	else if(mark==2){
																		int i;
																		int l=strlen(keep[1]);
																		for(i = 1;  i < l-1; i++) 
																			printf("%c",keep[1][i]);
																	}
																	else{
																		printf("\n");
																	}
																}
															}
		| elseif ELSE SBS ifbody SBE						{	if(ifdone[ifptr] == 0){
																	ifdone[ifptr] = 1;
																	if(mark==1)
																	{
																		printf("%d",v1[2]);
																	}
																	else if(mark==2)
																	{
																		int i;
																		int l=strlen(keep[2]);
																		for(i = 1;  i < l-1; i++) 
																			printf("%c",keep[2][i]);
																	}
																	else
																	{
																		printf("\n");
																	}
																}
															}

		;




/*------foor loop ----------*/


forloop	: FOR FBS expression TO expression COLON expression FBE SBS loopbody SBE 	
					{
						int start = $3;
						int end = $5;
						int dif = $7;
						int count = 0;
						int i = 0;
						finaldif=dif;
						for(i = start; i <= end; i += dif){
							count++;
						}	
						savestate = count;
						if(mark==1){
							int k=0;
							for(k = 1; k <= savestate; k += finaldif){
								printf("%d",v1[3]);
							}
						}
						else if(mark==2){
							int l = strlen(keep[3]);
							int k = 0,i=0;
							for(k = 1; k <= savestate; k += finaldif){
								for(i = 1;  i < l-1; i++) { 
									printf("%c",keep[3][i]);
								}
							}
						}
						else{
							int k = 0;
							for(k = 1; k <= savestate; k += finaldif){
								printf("\n");
							}
						}
						
					}
		;




/*------switch case --------*/


switch	: SWITCH FBS expswitch FBE SBS switchbody SBE 
		;

expswitch 	:  expression 				{	switchdone = 0;
											switchvar = $1;
										}
			;


switchbody	: /* empty */
				| switchbody expression COLON SBS loopbody SBE 	{	if($2 == switchvar){
																			if(mark==1){
																				printf("%d\n",v1[3]);
																			}
																			else if(mark==2){
																				int l = strlen(keep[3]);
																				int i=0;
																				for(i = 1;  i < l-1; i++) { 
																						printf("%c",keep[3][i]);
																					}
																			}
																			else{
																				printf("\n");
																			}
																			switchdone = 1;
																		}					
																	}
				| switchbody DEFAULT COLON SBS loopbody SBE 		{	if(switchdone == 0){
																			if(mark==1){
																				printf("%d\n",v1[3]);
																			}
																			else if(mark==2){
																				int l = strlen(keep[3]);
																				int i=0;
																				for(i = 1;  i < l-1; i++) { 
																						printf("%c",keep[3][i]);
																					}
																			}
																			else{
																				printf("\n");
																			}
																			switchdone = 1;
																		}
																	}
				;




/*------function-----------*/


function 	: /* empty */
			| function func
			;

func 	: type UDFUNCTION FBS fparameter FBE SBS statement SBE	{	printf("USER DEFINED Function found\n");  }
		;
fparameter 	: /* empty */
			| type ID fsparameter
			;
fsparameter : /* empty */
			| fsparameter COMMA type ID
			;

%%

int yyerror(char *s){
	printf( "\nError on Line %d:\n %s\n",yylineno, s);
}