%{
	#include "1907103.tab.h"
	#include <string.h>
	#include <stdlib.h>
	int yyparse();
	int yylex();
	int yyerror();
%}

letter [a-zA-Z]
digit [0-9]
special [$_@]
newline [\n]
space [ \t]

%option yylineno

%%

"!include"{space}+{letter}+{digit}*"!"\n 	{ /*header*/ printf("header file found\n");}
"$$".*\n 				{ /*This is comment*/ }
"//"[^$]*"//" 			{ /*multi line comment*/ }
"," 		 			{ return COMMA;}
";"			 			{ return SEMICOLON;}
"beg" 					{ return BEG;}
{digit}+ 				{ yylval.value = atoi(yytext); return NUMBER;}
"int" 					{ return INT;}
"double" 				{ return DOUBLE;}
"char" 					{ return CHAR;}

"printvar" 				{ return PRINTVAR;}
"printstr" 				{ return PRINTSTR;}
"newline" 				{ return PRINTLN;}
"printfunc"					{ return PRINTFUNC;}

"function"{space}+{letter}+ 		{ return UDFUNCTION;}

"{" 			{return SBS;}
"}" 			{return SBE;}
"(" 			{return FBS;}
")" 			{return FBE;}
"=" 			{return ASSIGN;}
"+" 			{return PLUS;}
"-" 			{return MINUS;}
"*" 			{return MULTIPLY;}
"/" 			{return DIVIDE;}
"%" 			{return MOD;}
"<" 			{return LESSTHAN;}
">" 			{return GREATERTHAN;}
"<=" 			{return LESSEQUAL;}
">=" 			{return GREATEREQUAL;}
"===" 			{return EQUAL;}
"!==" 			{return NOTEQUAL;}

"MAX"  			{return MAX;}
"MIN"  			{return MIN;}
"compmax" 		{return COMPAREMAX;}
"compmin" 		{return COMPAREMIN;}
"REVSTR" 		{return REVERSE;}
"SORT" 			{return SORT;}
"FACT"			{return FACT;}
"ODDEVEN"		{return ODDEVEN;}
"SUMDIGIT"		{return SUMDIGIT;}
"REVNUM"		{return REVNUM;}
"sin" 			{return SINFUNC;}
"cos" 			{return COSFUNC;}
"tan" 			{return TANFUNC;}
"log10"			{return LOG10FUNC;}
"log" 			{return LOGFUNC;}
"gcd" 			{return GCDFUNC;}
"lcm" 			{return LCMFUNC;}
"^" 			{return POWERFUNC;}

"jodi"			{return IF;}
"nahole"		{return ELSE;}
"naholejodi"	{return ELSEIF;}

"for"			{return FOR;}
"to"			{return TO;}
":"				{return COLON;}

"switch"		{return SWITCH;}
"default"		{return DEFAULT;}


"\""(.)*"\"" 	{strcpy(yylval.text,yytext);return STR; /* \ is the esc seq */}
({letter}|{digit}|{special})+ 		{ strcpy(yylval.text,yytext); return ID;}
{space}|{newline} 	{}
. 				{printf("ERROR ON LINE %d : \n Invalid Token \n", yylineno);}


%%
int yywrap() {return 1;}
int main(){
	yyin = freopen("in.txt","r",stdin);
	freopen("out.txt","w",stdout);
	yyparse();
}