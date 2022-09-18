/* definitions */

%{
#include<stdio.h>
#include "tokendef.h"

int scancol = 1;
int yycol = 1;
int scanlineno = 1;

char* yyerror;

void updateCol(); //TODO: Implement this function to keep track of column numbers
void countLines(); //TODO: Implement this function to keep track of line numbers
int processString(); //TODO: Implement this function to check for illegal escape sequences in string literals and print out escape sequences correctly.
%}

newline         [\n]
whitespace      [ \t]
integer         0|[1-9]{digit}*
character       [a-zA-Z] 

/*Gunnar character only includes letters it might need changes*/

/* String can contain any characters between the double quotes other than a newline or unescaped doublequotes.
*/
string       \".*\"

/* multiline string has \n somewhere in the middle */
multlnstring //TODO

/* If the end quote is found, one of the two regexes above will be matched. Otherwise,the string is unterminated.
*/
untermstring \"

comment        	"//".*                                  
multlncomment   [/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]
untermcomment   [/][*]

identifier      {letter}+
illidentifier   [0-9]+[a-zA-Z][a-zA-Z0-9]*

yylineno = 1;

%%

 /* rules */

 /* Keywords */;
"if"            {updateCol(); return KWD_IF;}
"else"			{updateCol(); return KWD_ELSE;}
"while"			{updateCol(); return KWD_WHILE;}
"int"			{updateCol(); return KWD_INT;}
"string"		{updateCol(); return KWD_STRING;}
"char"			{updateCol(); return KWD_CHAR;}
"return"		{updateCol(); return KWD_RETURN;}
"void"          {updateCol(); return KWD_VOID;}

 /* Operators */;
\+              {updateCol(); return OPER_ADD;}
\-				{updateCol(); return OPER_SUB;}
\*				{updateCol(); return OPER_MUL;}				
\/				{updateCol(); return OPER_DIV;}
\%				{updateCol(); return OPER_MOD;}
\<				{updateCol(); return OPER_LE;}
\>				{updateCol(); return OPER_GT;}
\<\=				{updateCol(); return OPER_LTE;}
\>\=				{updateCol(); return OPER_GTE;}
\=\=				{updateCol(); return OPER_EQ;}
\!\=				{updateCol(); return OPER_NEQ;}
\=				{updateCol(); return OPER_ASGN;}
\[				{updateCol(); return LSQ_BRKT;}
\]				{updateCol(); return RSQ_BRKT;}
\{				{updateCol(); return LCRLY_BRKT;}
\}				{updateCol(); return RCRLY_BRKT;}
\(				{updateCol(); return LPAREN;}
\)				{updateCol(); return RPAREN;}
\,				{updateCol(); return COMMA;}
\;				{updateCol(); return SEMICLN;}
\@				{updateCol(); return OPER_AT;}
\+\+				{updateCol(); return OPER_INC;}
\-\-				{updateCol(); return OPER_DEC;}
\&\&				{updateCol(); return OPER_AND;}
\|\|				{updateCol(); return OPER_OR;}
\!				{updateCol(); return OPER_NOT;}

 /* Identifiers */;
{identifier}    {return ID; }
{illidentifier} //TODO

 /* Constants */;
{integer}       {updateCol(); return INTCONST;}
{integerlead0}  //TODO: "Integers may not have leading zeros"
{character}     {updateCol(); return CHARCONST;}
{string}        {updateCol(); return processString();}
{untermstring}  {updateCol(); yyerror = "Unterminated string"; return ERROR;}
{multlnstring}  {updateCol(); countLines(); yyerror = "String spans multiple lines"; return ERROR;} // Catching a multi-line string and generating an error.

 /* Comments */;
{comment}       {updateCol(); /* DO NOTHING */ }
{untermcomment} {updateCol(); yyerror = "Unterminated comment"; return ERROR;}
{multlncomment} {countLines();updateCol(); /* DO NOTHING */ }

 /* Other */;
{newline}       {countLines();}
{whitespace}    {updateCol(); /* skip whitespace */}
.               {return ILLEGAL_TOK;}

%%

/* user routines */

void updateCol(){
	yycol = scancol;
    scancol += yyleng; //make scancol = length of lexeme
}

void countLines(){
	scanlineno++;  //add 1 to line num
	yylineno = scanlineno; //update yylineno
	yycol = 1;    //new line so make column = 1 again 
}


int processString(){
	// TODO: Process a string literal and return the appropriate microsyntax.
	return STRCONST;
}
