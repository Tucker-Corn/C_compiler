%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<../src/tree.h>
#include<../src/strtab.h>

extern int yylineno;
/* nodeTypes refer to different types of internal and external nodes that can be part of the abstract syntax tree.*/
enum nodeTypes {PROGRAM, DECLLIST, DECL, VARDECL, TYPESPEC, FUNDECL,
                FORMALDECLLIST, FORMALDECL, FUNBODY, LOCALDECLLIST,
                STATEMENTLIST, STATEMENT, COMPOUNDSTMT, ASSIGNSTMT,
                CONDSTMT, LOOPSTMT, RETURNSTMT, EXPRESSION, RELOP,
                ADDEXPR, ADDOP, TERM, MULOP, FACTOR, FUNCCALLEXPR,
                ARGLIST, INTEGER, IDENTIFIER, VAR, ARRAYDECL, CHAR,
                FUNCTYPENAME};

enum opType {ADD, SUB, MUL, DIV, LT, LTE, EQ, GTE, GT, NEQ};


tree *ast;
/* NOTE: mC has two kinds of scopes for variables : local and global. Variables declared outside any
function are considered globals, whereas variables (and parameters) declared inside a function foo are local to foo. You should update the scope variable whenever you are inside a production that matches function definition (funDecl production). The rationale is that you are entering that function, so all variables, arrays, and other functions should be within this scope. You should pass this variable whenever you are calling the ST_insert or ST_lookup functions. This variable should be updated to scope = "" to indicate global scope whenever funDecl finishes. Treat these hints as helpful directions only. You may implement all of the functions as you like and not adhere to my instructions. As long as the directory structure is correct and the file names are correct, we are okay with it. */
char* scope = "";
%}

/* the union describes the fields available in the yylval variable */
%union
{
    int value;
    struct treenode *node;
    char *strval;
}

/*Add token declarations below. The type <value> indicates that the associated token will be of a value type such as integer, float etc., and <strval> indicates that the associated token will be of string type.*/
%token <strval> ID
%token <value> INTCONST
/* TODO: Add the rest of the tokens below.*/
%token <value> KWD_INT
%token <value> KWD_CHAR
%token <value> KWD_VOID
%token <value> KWD_IF
%token <value> KWD_ELSE
%token <value> KWD_WHILE
%token <value> KWD_RETURN
%token <value> CHARCONST
%token <strval> STRCONST
%token <value> LPAREN RPAREN 
%token <value> LBRACKET RBRACKET
%token <value> ASSIGN_OP 
%token <value> ADD_OP 
%token <value> SEMI_COLON 
%token <value> COMMA 
/* TODO: Declate non-terminal symbols as of type node. Provided below is one example. node is defined as 'struct treenode *node' in the above union data structure. This declaration indicates to parser that these non-terminal variables will be implemented using a 'treenode *' type data structure. Hence, the circles you draw when drawing a parse tree, the following lines are telling yacc that these will eventually become circles in an AST. This is one of the connections between the AST you draw by hand and how yacc implements code to concretize that. We provide with two examples: program and declList from the grammar. Make sure to add the rest.  */

%type <node> program declList decl varDecl typeSpecifier funDecl formalDeclList formalDecl funBody localDeclList statementList statement compoundStmt assignStmt condStmt loopStmt returnStmt var expression relop addExpr addop term mulop factor funcCallExpr argList





*/
%start program

%%
/* TODO: Your grammar and semantic actions go here. We provide with two example productions and their associated code for adding non-terminals to the AST.*/

program         : declList
                 {
                    tree* progNode = maketree(PROGRAM);
                    addChild(progNode, $1);
                    ast = progNode;
                 }
                ;

declList        : decl
                 {
                    tree* declListNode = maketree(DECLLIST);
                    addChild(declListNode, $1);
                    $$ = declListNode;
                 }
                | declList decl
                 {
                    tree* declListNode = maketree(DECLLIST);
                    addChild(declListNode, $1);
                    addChild(declListNode, $2);
                    $$ = declListNode;
                 }
                ;

decl            : varDecl
                 {

                 }
                ;
                | funDecl
                 {

                 }
                ;

varDecl         : typeSpecifier ID LBRACKET INTCONST RBRACKET SEMI_COLON
                 {
                
                 }
                ;
                | typeSpecifier ID SEMI_COLON
                 {
                    printf("%s\n", yylval.strval);
		            tree *vardeclNode = maketree(VARDECL);
                    addChild(declNode, $1);
		            $$ = vardeclNode;
                 }
                ;

typeSpecifier   : KWD_INT
                 {
                    $$ = maketreeWithVal(TYPESPEC, INT_TYPE);
                 }
                ;
                | KWD_CHAR
                 {
                    $$ = maketreeWithVal(TYPESPEC, CHAR_TYPE);
                 }
                ;
                | KWD_VOID
                 {
                    $$ = maketreeWithVal(TYPESPEC, VOID_TYPE);
                 }
                ;

funDecl          : typeSpecifier ID LPAREN formalDeclList RPAREN funBody
                {

                }
                ;
                | typeSpecifier ID LPAREN RPAREN funBody
                {

                }
                ;

formalDeclList  : formalDecl
                {

                }
                ;
                | formalDecl COMMA formalDeclList
                {

                }
                ;

formalDecl      : typeSpecifier ID
                {

                }
                ;
                | typeSpecifier ID LBRACKET RBRACKET
                {

                }
                ;

funBody         : {localDeclList statementList }
                {

                }
                ;

localDeclList :
                | varDecl localDeclList
                {
                     
                }
                ;

statementList :
                | statement statementList
                {

                }
                ;

statement        : compoundStmt
                {

                }
                ;
                | assignStmt
                {

                }
                ;
                | condStmt
                {

                }
                ;
                | loopStmt
                {

                }
                ;
                | returnStmt
                {

                }
                ;

compoundStmt    : {statementList }
                {

                }
                ;

assignStmt      : var EQ expression SEMI_COLON
                {

                }
                ;
                | expression  SEMI_COLON
                {

                }
                ;

condStmt        : KWD_IF LPAREN expression RPAREN statement
                {

                }
                ;
                | KWD_IF LPAREN expression RPAREN statement KWD_ELSE statement
                {

                }
                ;

loopStmt        : KWD_WHILE LPAREN expression RPAREN statement
                {

                }
                ;

returnStmt      : KWD_RETURN SEMI_COLON
                {

                }
                ;
                | KWD_RETURN expression SEMI_COLON
                {

                }
                ;

var             : ID
                {
                    $$ = maketreeWithVal(IDENTIFIER, $1);
                }
                ;
                | ID LBRACKET addExpr RBRACKET
                {

                }
                ;

expression      : addExpr
                {

                }
                ;
                | expression relop addExpr
                {

                }
                ;

relop            : LTE
                {

                }
                ;
                | LT
                {

                }
                ;
                | GT
                {

                }
                ;
                | GTE
                {

                }
                ;
                | ==
                {

                }
                ;
                | NEQ
                {

                }
                ;

addExpr         : term
                {

                }
                ;
                | addExpr addop term
                {

                }
                ;

addop            : ADD
                {

                }
                ;
                | SUB
                {

                }
                ;

term             : factor
                {

                }
                ;
                | term mulop factor
                {

                }
                ;

mulop           : MUL
                {

                }
                ;
                | DIV
                {

                }
                ;

factor          : LPAREN expression RPAREN
                {

                }
                ;
                | var
                {

                }
                ;
                | funcCallExpr
                {

                }
                ;
                | INTCONST
                {

                }
                ;
                | CHARCONST
                {

                }
                ;
                | STRCONST
                {

                }
                ;

funcCallExpr    : ID LPAREN argList RPAREN
                {

                }
                ;
                | ID LPAREN RPAREN
                {

                }
                ;

argList          : expression
                {

                }
                ;
                | argList COMMA expression
                {

                }
                ;
%%

int yywarning(char * msg){
    printf("warning: line %d: %s\n", yylineno, msg);
    return 0;
}

int yyerror(char * msg){
    printf("error: line %d: %s\n", yylineno, msg);
    return 0;
}
