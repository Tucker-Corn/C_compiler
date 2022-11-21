
/* definitions */

%{
#include<stdio.h>
#include"../obj/y.tab.h"
//#include "tokendef.h"
#define ERROR 301
#define ILLEGAL_TOK 302


int scancol = 1;
int yycol = 1;
int scanlineno = 1;

char* yyerrormsg;
char* yyerror;
void updateCol();
void countLines();
int processChar();
%}

newline         \n
whitespace      [ \t]+
integer         0|[1-9][0-9]*
integerlead0    0[0-9]+
character       \'([^\']|\\[\\nt\'])\'


comment         \/\*\/?([^\n\/]|[^\*]\/)*\*\/
multlncomment   \/\*\/?([^\/]|[^\*]\/)*\n([^\/]|[^\*]\/)*\*\/
untermcomment   \/\*\/?([^\/\n]|[^\*]\/)*

identifier      [a-zA-Z][a-zA-Z0-9]*
illidentifier   [0-9]+[a-zA-Z][a-zA-Z0-9]*

yylineno = 1;

%%

 /* rules */

 /* Keywords */;
"if"            {updateCol(); return KWD_IF;}
"else"          {updateCol(); return KWD_ELSE;}
"while"         {updateCol(); return KWD_WHILE;}
"int"           {updateCol(); return KWD_INT;}
"char"          {updateCol(); return KWD_CHAR;}
"return"        {updateCol(); return KWD_RETURN;}
"void"          {updateCol(); return KWD_VOID;}

 /* Operators */;
\+              {updateCol(); return ADD_OP;}
\-              {updateCol(); return SUB_OP;}
\*              {updateCol(); return MUL_OP;}
\/              {updateCol(); return DIV_OP;}
\<=             {updateCol(); return LTE_OP;}
\>=             {updateCol(); return GTE_OP;}
\<              {updateCol(); return LT_OP;}
\>              {updateCol(); return GT_OP;}
==              {updateCol(); return EQ_OP;}
!=              {updateCol(); return NEQ_OP;}
=               {updateCol(); return ASSIGN_OP;}

 /* Punctuation */;
\[              {updateCol(); return LBRACKET;}
\]              {updateCol(); return RBRACKET;}
\{              {updateCol(); return LCURLY;}
\}              {updateCol(); return RCURLY;}
\(              {updateCol(); return LPAREN;}
\)              {updateCol(); return RPAREN;}
,               {updateCol(); return COMMA;}
;               {updateCol(); return SEMI_COLON;}

 /* Identifiers */;
{identifier}    {updateCol();
                 yylval.strval = (char *) malloc(yyleng *sizeof(char));
                 strcpy(yylval.strval, yytext);
                 return ID;}
{illidentifier} {updateCol(); yyerrormsg = "Identifiers may not start with a digit"; return ERROR;}

 /* Constants */;
{integer}       {updateCol(); yylval.value = atoi(yytext); return INTCONST;}
{integerlead0}  {updateCol(); yyerrormsg = "Integers may not have leading zeros"; return ERROR;}
{character}     {updateCol(); return processChar();}

 /* Comments */;
{comment}       {updateCol(); /* skip comments */}
{untermcomment} {updateCol(); yyerrormsg = "Unterminated comment"; return ERROR;}
{multlncomment} {updateCol(); countLines(); yyerrormsg = "Comment spans multiple lines"; return ERROR;}

 /* Number */;   {yycol+=yyleng; yylval.value = atoi(yytext); return INTCONST;}

 /* Other */;
{newline}       {scancol = 1; scanlineno++; yylineno = scanlineno;}
{whitespace}    {updateCol(); /* skip whitespace */}
.               {return ILLEGAL_TOK;}

%%

/* user routines */

void updateCol(){
    yycol = scancol;
    scancol += yyleng;
}

void countLines(){
    yylineno = scanlineno;
    for (int i=0; i<yyleng; i++){
        if (yytext[i] == '\n')
            scanlineno++;
    }
}

int processChar()
{
    // yytext[0] will be "'", so check yytext[1] for escape
    if (yytext[1] == '\\')
    {
        if (yytext[2] == '\'')
        {
            yylval.value = '\'';
        }
        else if (yytext[2] == 'n')
        {
            yylval.value = '\n';
        }
        else if (yytext[2] == 't')
        {
            yylval.value = '\t';
        }
        else if (yytext[2] == '\\')
        {
            yylval.value = '\\';
        }
        else{
            yyerrormsg = "Unrecognized escape character in String";
            return ERROR;
        }
    }
    else{
        // If the character isn't escaped, add it to our temp string.
        yylval.value = yytext[1];
    }
    return CHARCONST;
}
