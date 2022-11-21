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
%token <value> KWD_INT//
%token <value> KWD_CHAR//
%token <value> KWD_VOID//
%token <value> KWD_IF//
%token <value> KWD_ELSE//
%token <value> KWD_WHILE//
%token <value> KWD_RETURN//
%token <value> CHARCONST//
%token <strval> STRCONST

%token <value> LPAREN RPAREN 
%token <value> LBRACKET RBRACKET
%token <value> LCURLY RCURLY
%token <value> ASSIGN_OP 
%token <value> EQ_OP NEQ_OP
%token <value> LTE_OP LT_OP GTE_OP GT_OP
%token <value> ADD_OP SUB_OP
%token <value> MUL_OP DIV_OP
%token <value> SEMI_COLON 
%token <value> COMMA 

/* TODO: Declate non-terminal symbols as of type node. Provided below is one example. node is defined as 'struct treenode *node' in the above union data structure. This declaration indicates to parser that these non-terminal variables will be implemented using a 'treenode *' type data structure. Hence, the circles you draw when drawing a parse tree, the following lines are telling yacc that these will eventually become circles in an AST. This is one of the connections between the AST you draw by hand and how yacc implements code to concretize that. We provide with two examples: program and declList from the grammar. Make sure to add the rest.  */

%type <node> program  
%type <node>declList 
%type <node>decl 
%type <node>varDecl 
%type <node>typeSpecifier 
%type <node>funDecl 
%type <node>formalDeclList 
%type <node>formalDecl  
%type <node>funBody 
%type <node>localDeclList 
%type <node>statementList 
%type <node>statement 
%type <node>compoundStmt 
%type <node>assignStmt 
%type <node>condStmt 
%type <node>loopStmt 
%type <node>returnStmt 
%type <node>var 
%type <node>expression 
%type <node>relop 
%type <node>addExpr 
%type <node>addop 
%type <node>term 
%type <node>mulop 
%type <node>factor 
%type <node>funcCallExpr 
%type <node>argList




%start program

%%
/* TODO: Your grammar and semantic actions go here. We provide with two example productions and their associated code for adding non-terminals to the AST.*/


program         : declList 
                 {
                    tree *progNode = maketree(PROGRAM);
                    addChild(progNode, $1);
                    ast = progNode;
                 }
                ;
decl            : varDecl 
                 {
                    tree *declNode = maketree(DECL);
                    addChild(declNode, $1);
                    $$ = declNode; 
                 }
                | funDecl ID SEMI_COLON
                 {
                    tree *declNode = maketree(VARDECL);
                    addChild(declNode, $1);
                    $$ = declNode; 
                 }
                ;
declList        : decl
                 {
                    tree *declListNode = maketree(DECLLIST);
                    addChild(declListNode, $1);
                    $$ = declListNode;
                 }
                | declList decl
                 {
                    tree *declListNode = maketree(DECLLIST);
                    addChild(declListNode, $1);
                    addChild(declListNode, $2);
                    $$ = declListNode;
                 }
                ;



varDecl         : typeSpecifier ID LBRACKET INTCONST RBRACKET SEMI_COLON
                 {
                    printf("%s\n", yylval.strval);
                    tree *declNode = maketree(VARDECL);
                    addChild(declNode, $1);
                    $$ = declNode;
                 }
                | typeSpecifier ID SEMI_COLON
                 {
                    printf("%s\n", yylval.strval);
		            tree *vardeclNode = maketree(VARDECL);
                    addChild(vardeclNode, $1);
		            $$ = vardeclNode;
                 }
                ;

typeSpecifier   : KWD_INT
                 {
                    $$ = maketreeWithVal(TYPESPEC, INT_TYPE);
                 }
                | KWD_CHAR
                 {
                    $$ = maketreeWithVal(TYPESPEC, CHAR_TYPE);
                 }
                | KWD_VOID
                 {
                    $$ = maketreeWithVal(TYPESPEC, VOID_TYPE);
                 }
                ;

funDecl          : typeSpecifier ID LPAREN formalDeclList RPAREN funBody
                {
					scope++;
                }
                | typeSpecifier ID LPAREN RPAREN funBody
                {
                	
					scope--;
                }
                ;

formalDeclList  : formalDecl
                {
                    tree *formaldeclNode = maketree(FORMALDECL);
                    addChild(formaldeclNode, $1);
                    $$ = formaldeclNode;
                }
                | formalDecl COMMA formalDeclList
                {
                      tree* formaldeclNode = maketree(FORMALDECL);
                      addChild(formaldeclNode, $1);
                      
                      /*Fix later*/
                      //addChild(formalDeclListNode, $3);
                      $$ = formaldeclNode;
                }
                ;

formalDecl      : typeSpecifier ID
                {
                 tree *declNode = maketree(FORMALDECL);
                 addChild(declNode, $1);
                 addChild(declNode, maketreeWithVal(IDENTIFIER,$1));
                 $$ = declNode;

                }
                | typeSpecifier ID LBRACKET RBRACKET
                {
                  tree *declNode = maketree(FORMALDECL);
                  addChild(declNode, $1);
                  addChild(declNode, maketreeWithVal(IDENTIFIER,$1));
                  $$ = declNode;
                }
                ;

funBody         : LCURLY localDeclList statementList RCURLY
                {
                   tree *funBodyNode = maketree(FUNBODY);
                   addChild(funBodyNode, $2);
                   addChild(funBodyNode, $3);
                   scope--;
                   $$ = funBodyNode;
                }
                ;

localDeclList :
                {
                        tree *localDeclListNode = maketree(LOCALDECLLIST);
                        $$ = localDeclListNode;
                }
                | varDecl localDeclList
                {
                 tree *localDeclListNode = maketree(LOCALDECLLIST);
                 addChild(localDeclListNode, $1);
                 addChild(localDeclListNode, $2);
                 $$ = localDeclListNode;
                }
                ;

statementList :
                {
                        tree *statementNode = maketree(STATEMENTLIST);
                        $$ = statementNode;
                }
                | statement statementList
                {
                        tree *statementNode = maketree(STATEMENTLIST);
                        addChild(statementNode, $1);
                        addChild(statementNode, $2);
                        $$ = statementNode;
                }
                ;

statement        : compoundStmt
                {
                    tree* statementNode = maketree(STATEMENT);
                    addChild(statementNode, $1);
                    $$ = statementNode;
                }
                | assignStmt
                {
                    tree* statementNode = maketree(STATEMENT);
                    addChild(statementNode, $1);
                    $$ = statementNode;
                }
                | condStmt
                {
                    tree* statementNode = maketree(STATEMENT);
                    addChild(statementNode, $1);
                    $$ = statementNode;
                }
                | loopStmt
                {
                    tree* statementNode = maketree(STATEMENT);
                    addChild(statementNode, $1);
                    $$ = statementNode;
                }
                | returnStmt
                {
                    tree* statementNode = maketree(STATEMENT);
                    addChild(statementNode, $1);
                    $$ = statementNode;
                }
                ;

compoundStmt    : LCURLY statementList RCURLY
                {
                    tree *compoundStmtNode = maketree(COMPOUNDSTMT);
                    addChild(compoundStmtNode, $3);
                    $$ = compoundStmtNode;
                }
                ;

assignStmt      : var EQ_OP expression SEMI_COLON
                {
						tree *assignStmtNode = maketree(ASSIGNSTMT);
						addChild(assignStmtNode, $1);
						addChild(assignStmtNode, maketree(OPER_ASGN));
						addChild(assignStmtNode, $3);
						addChild(assignStmtNode, maketree(SEMICLN));

						tree *tempVar = assignStmtNode->children[0];
						while (tempVar->numChildren > 0) {
							tempVar = tempVar->children[0];
						}

						tree *tempExpr = assignStmtNode->children[2];
						while (tempExpr->numChildren > 0) {
							tempExpr = tempExpr->children[0];
						}
						
						int varType = symbolTable[tempVar->val].type;
						if (varType > 0) {
						  int expType = -1;
						  if (tempExpr->nodeKind == T_ID) {
						    if (symbolTable[tempExpr->val].type == FUNCTION) {
						      expType = symbolTable[tempExpr->val].returnType;
						    } else {
						      expType = symbolTable[tempExpr->val].type;
						    }
						  } else {  // if constant
						    expType = tempExpr->nodeKind;
						  }
					    
					      if (!((varType == INT_TYPE || varType == T_INTARRAY) && (expType == INT_TYPE || expType == INTCONST || expType == T_INTARRAY)) &&
							  !((varType == VOID_TYPE || varType == T_VOIDARRAY) && (expType == VOID_TYPE || expType == T_VOIDARRAY)) &&
							  !((varType == CHAR_TYPE || varType == T_CHARARRAY) && (expType == CHAR_TYPE || expType == CHARCONST || expType == T_CHARARRAY)) &&
							  !(varType == T_CHARARRAY && expType == STRCONST)) {
						    printf("Error: %d: Type mismatch in assignment\n", yylineno);
						  }
						}

					    
						$$ = assignStmtNode;
					  }
                | expression  SEMI_COLON
                {
                    tree *assignmentNode = maketree(ASSIGNSTMT);
                    addChild(assignmentNode, $1);
                    $$ = assignmentNode;
                }
                ;

condStmt        : KWD_IF LPAREN expression RPAREN statement
                {
                   tree *condNode = maketree(CONDSTMT);
                   addChild(condNode, $3);
                   addChild(condNode, $5);
                   $$ = condNode;

                }
                | KWD_IF LPAREN expression RPAREN statement KWD_ELSE statement
                {
                   tree *condNode = maketree(CONDSTMT);
                   addChild(condNode, $3);
                   addChild(condNode, $5);
                   addChild(condNode, $7);
                   $$ = condNode;
                }
                ;

loopStmt        : KWD_WHILE LPAREN expression RPAREN statement
                {
                   tree *loopNode = maketree(LOOPSTMT);
                   addChild(loopNode, $3);
                   addChild(loopNode, $5);
                   $$ = loopNode;
                }
                ;

returnStmt      : KWD_RETURN SEMI_COLON
                {
                   tree* returnStmtNode = maketree(RETURNSTMT);
                   $$ = returnStmtNode;
                }
                | KWD_RETURN expression SEMI_COLON
                {
                tree *returnNode = maketree(RETURNSTMT);
                addChild(returnNode, $2);
                $$ = returnNode;
                }
                ;

var             : ID
                {
						tree *varNode = maketree(VAR);
						addChild(varNode, maketreeWithVal(IDENTIFIER, ST_lookup($1, scopeName)));
						$$ = varNode;
					  }
					| ID LBRACKET addExpr RBRACKET
					  {
						tree *varNode = maketree(VAR);
						int index = ST_lookup($1, scopeName);
						addChild(varNode, maketreeWithVal(IDENTIFIER, index));
						addChild(varNode, maketree(LBRACKET));
						addChild(varNode, $3);
						
						if (index >= 0) {
							tree *temp = varNode->children[2];
							while (temp->numChildren > 0) {
								temp = temp->children[0];
							}
							if (symbolTable[varNode->children[0]->val].data_type == INT_TYPE ||
								symbolTable[varNode->children[0]->val].data_type == CHAR_TYPE ||
								symbolTable[varNode->children[0]->val].data_type == VOID_TYPE) {
								printf("Error: %d: Indexing a scalar variable\n", yylineno);
							} else {
								if (temp->nodeKind == IDENTIFIER) {
								  if (symbolTable[temp->val].data_type == FUNCTION) {
									if (symbolTable[temp->val].returnType != INT_TYPE)
									  printf("Error: %d: Indexing an array variable with a non-integer type\n", yylineno);
								  } else {
									if (symbolTable[temp->val].data_type != INT_TYPE)
									  printf("Error: %d: Indexing an array variable with a non-integer type\n", yylineno);
								  }
								} else {  // if constant
								  if (temp->nodeKind != INTCONST) {
									printf("Error: %d: Indexing an array variable with a non-integer type\n", yylineno);
								  } else {
									int arrIndex = atoi(yylval.strval);
									if (arrIndex >= symbolTable[index].arrBound)
									  printf("Error: %d: Indexing an array with an out-of-bounds integer literal\n", yylineno);
								  }
								}
							}
						}
						
						addChild(varNode, maketree(RBRACKET));
						$$ = varNode;
					  }
					;

expression      : addExpr
                {
                    tree* ExprNode = maketree(EXPRESSION);
                    addChild(ExprNode, $1);
                    $$ = ExprNode;
                }
                | expression relop addExpr
                {
                        tree* ExprNode = maketree(EXPRESSION);
                        addChild(ExprNode, $1);
                        addChild(ExprNode, $2);
                        addChild(ExprNode, $3);
                        $$ = ExprNode;
                }
                ;

relop            : LTE_OP
                {
                    $$ = maketreeWithVal(RELOP, LTE);
                }
                | LT_OP
                {
                    $$ = maketreeWithVal(RELOP, LT);
                }
                | GT_OP
                {
                    $$ = maketreeWithVal(RELOP, GT);
                }
                | GTE_OP
                {
                    $$ = maketreeWithVal(RELOP, GTE);
                }
                | ASSIGN_OP
                {
                    $$ = maketreeWithVal(RELOP, ASSIGNSTMT);
                }
                | NEQ_OP
                {
                    $$ = maketreeWithVal(RELOP, NEQ);
                }
                ;

addExpr         : term
                {
                    tree *addExprNode = maketree(ADDEXPR);
                    addChild(addExprNode, $1);
                    $$ = addExprNode;
                }
                | addExpr addop term
                {
                   tree *addExprNode = maketree(ADDEXPR);
                   addChild(addExprNode, $1);
                   addChild(addExprNode, $2);
                   addChild(addExprNode, $3);
                   $$ = addExprNode;
                }
                ;

addop            : ADD_OP
                {
                    $$ = maketreeWithVal(ADDOP, ADD);
                }
                | SUB_OP
                {
                    $$ = maketreeWithVal(ADDOP, SUB);
                }
                ;

term             : factor
                {
                    tree* termNode = maketree(TERM);
                    addChild(termNode, $1);
                    $$ = termNode;
                }
                | term mulop factor
                {
                     tree *termNode = maketree(TERM);
                     addChild(termNode, $1);
                     addChild(termNode, $2);
                     addChild(termNode, $3);
                     $$ = termNode;
                }
                ;

mulop           : MUL_OP
                {
                    $$ = maketreeWithVal(MULOP, MUL);
                }
                | DIV_OP
                {
                    $$ = maketreeWithVal(MULOP, DIV);
                }
                ;

factor          : LPAREN expression RPAREN
                {
                     tree *factorNode = maketree(FACTOR);
                     addChild(factorNode, $2);
                     $$ = factorNode;
                }
                | var
                {
                    tree *factorNode = maketree(FACTOR);
                    addChild(factorNode, $1);
                    $$ = factorNode;
                }
                | funcCallExpr
                {
                    tree *factorNode = maketree(FACTOR);
                    addChild(factorNode, $1);
                    $$ = factorNode;
                }
                | INTCONST
                {
                   $$ = maketreeWithVal(INTCONST, $1);
                }
                | CHARCONST
                {
                   $$ = maketreeWithVal(CHARCONST, $1);
                }
                | STRCONST
                {
                   $$ = maketreeWithVal(STRCONST, $1);
                }
                ;

funcCallExpr    : ID LPAREN argList RPAREN
                {
					    funcCallIndex++;
						tree *funcCallExprNode = maketree(FUNCCALLEXPR);
						funcCallAddr[funcCallIndex] = funcCallExprNode;
						int index = ST_lookup($1, "global");
						addChild(funcCallAddr[funcCallIndex], maketreeWithVal(INDENTIFIER, index));
						addChild(funcCallAddr[funcCallIndex], maketree(LPAREN));
						funcCallArr[funcCallIndex] = index;
					  }
					   argList RPAREN
					  {
						addChild(funcCallAddr[funcCallIndex], $4);
						addChild(funcCallAddr[funcCallIndex], maketree(RPAREN));
						$$ = funcCallAddr[funcCallIndex];
						
						int k, origType, callType, origArgs, callArgs = 0, mismatch = 0;
						int args[MAXARGS];
						while(!isEmpty(funcCallStack) && top(funcCallStack) == funcCallIndex) {
						  callArgs++;
						  args[callArgs-1] = funcCallStack->type;
						  funcCallStack = pop(funcCallStack);
						}
						
						origArgs = symbolTable[funcCallArr[funcCallIndex]].argsCount;
						
						if (funcCallAddr[funcCallIndex]->children[0]->val >= 0) {
						  if (origArgs != callArgs) {
						    if (callArgs - origArgs > 0) {
						      printf("Error: %d: Function signature mismatch: too many parameters\n", yylineno);
						    } else {
						      printf("Error: %d: Function signature mismatch: too few parameters\n", yylineno);
						    }
						  } else {
						    for (k = callArgs-1; k >= 0; k--) {
						      origType = symbolTable[funcCallArr[funcCallIndex]].args[k];
						      callType = args[callArgs-k-1];
						      if (!((origType == INT_TYPE || origType == T_INTARRAY) && (callType == INT_TYPE || callType == T_INTCONST || callType == T_INTARRAY)) &&
							      !((origType == VOID_TYPE || origType == T_VOIDARRAY) && (callType == VOID_TYPE || callType == T_VOIDARRAY)) &&
							      !((origType == CHAR_TYPE || origType == T_CHARARRAY) && (callType == CHAR_TYPE || callType == T_CHARCONST || callType == T_CHARARRAY)) &&
							      !(origType == CHAR_TYPE && callType == STRCONST)) {
							    mismatch++;
						      }
						    }
						  }
						
						  if (mismatch > 0) {
						    printf("Error: %d: Function declaration/call mismatch: %d argument(s) mismatch\n", yylineno, mismatch);
						  }
						}
						
					    funcCallIndex--;
					  }
					| ID LPAREN RPAREN
					  {
						tree *funcCallExprNode = maketree(FUNCCALLEXPR);
						int index = ST_lookup($1, "global");
						addChild(funcCallExprNode, maketreeWithVal(INDENTIFIER, index));
						addChild(funcCallExprNode, maketree(LPAREN));
						addChild(funcCallExprNode, maketree(RPAREN));
						$$ = funcCallExprNode;
						
						if (symbolTable[index].argsCount > 0) {
						    printf("Error: %d: Function signature mismatch: too few parameters\n", yylineno);
						}
					  }
					;

argList          : expression
                {
                    tree *argListNode = maketree(ARGLIST);
                    addChild(argListNode, $1);
                    $$ = argListNode;
                }
                | argList COMMA expression
                {
                       tree *argList = maketree(ARGLIST);
                       addChild(argList, $1);
                       addChild(argList, $3);
                       $$ = argList;
                }
                ;
%%

int yywarning(char *msg){
    printf("warning: line %d: %s\n", yylineno, msg);
    return 0;
}

int yyerror(char *msg){
    printf("error: line %d: %s\n", yylineno, msg);
    return 0;
}
