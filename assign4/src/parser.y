%{
#include<stdio.h>
#include<tree.h>
#include<table.h>
#include<stack.h>
#include<string.h>
#include<stdlib.h>
#include<../src/tree.h>
#include<../src/strtab.h>

#define MAXCALLS 100

extern int yylineno;

tree *ast;    //
char* scope = "";//

int scopeLevel = 0;
char scopeName[STRSIZE];
tree *funDeclNode;
tree *funcCallAddr[MAXCALLS];
int funcCallArr[MAXCALLS];
int funcCallIndex = -1;
stack* funcCallStack;

%}

%union 
{
  int value;
  struct treenode *node;
  char *strval;
}

%nonassoc LOWEST_PRECEDENCE
%nonassoc KWD_ELSE

%token <value> KWD_IF KWD_ELSE
%token <value> KWD_WHILE
%token <value> KWD_INT KWD_STRING KWD_CHAR
%token <value> KWD_RETURN KWD_VOID
%token <value> OPER_ADD OPER_SUB
%token <value> OPER_MUL OPER_DIV
%token <value> OPER_LT OPER_GT
%token <value> OPER_GTE OPER_LTE
%token <value> OPER_EQ OPER_NEQ
%token <value> OPER_ASGN
%token <value> LSQ_BRKT RSQ_BRKT
%token <value> LCRLY_BRKT RCRLY_BRKT
%token <value> LPAREN RPAREN
%token <value> COMMA SEMICLN
%token <strval> INTCONST
%token <value> CHARCONST
%token <strval> ID STRCONST
%token <value> ERROR
%token <value> ILLEGAL_TOK

%type <node> program declList decl varDecl typeSpecifier funDecl
%type <node> formalDeclList formalDecl funBody localDeclList
%type <node> statementList statement compoundStmt assignStmt condStmt
%type <node> loopStmt returnStmt var expression relop addExpr addop
%type <node> term mulop factor funcCallExpr argList


%start program 

%%

program				: declList
					  {
						tree *progNode = maketree(PROGRAM);
						addChild(progNode, $1);
						ast = progNode;
					  }
					;

declList  			: decl
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
					
decl				: varDecl
					  {
						tree *declNode = maketree(DECL);
						addChild(declNode, $1);
						$$ = declNode;
					  }
					| funDecl
					  {
						tree *declNode = maketree(DECL);
						addChild(declNode, $1);
						$$ = declNode;
					  }
					;
					
varDecl				: typeSpecifier ID LSQ_BRKT INTCONST RSQ_BRKT SEMICLN
					  {
					    int index = -1;
					    int type = 0;
						tree *varDeclNode = maketree(VARDECL);
						addChild(varDeclNode, $1);
						
						switch (varDeclNode->children[0]->nodeKind) {
							case T_INTEGER:
									type = T_INTARRAY;
									break;
							case T_CHARACTER:
									type = T_CHARARRAY;
									break;
							case T_VOID:
									type = T_VOIDARRAY;
									break;
							default:
									type = 0;
						}
						
						if (scopeLevel == 0) {
							index = ST_insert($2, "global", type, 0, 0, NULL, yylineno, atoi(yylval.strval));
						} else {
							index = ST_insert($2, scopeName, type, 0, 0, NULL, yylineno, atoi(yylval.strval));
						}
						addChild(varDeclNode, maketreeWithVal(T_ID, index));
						addChild(varDeclNode, maketree(T_LSQ_BRKT));
						addChild(varDeclNode, maketreeWithVal(T_INTCONST, atoi($4)));
						addChild(varDeclNode, maketree(T_RSQ_BRKT));
						addChild(varDeclNode, maketree(T_SEMICLN));
						$$ = varDeclNode;
					  }
					| typeSpecifier ID SEMICLN
					  {
					    int index = -1;
						tree *varDeclNode = maketree(VARDECL);
						addChild(varDeclNode, $1);
						if (scopeLevel == 0) {
							index = ST_insert($2, "global", varDeclNode->children[0]->nodeKind, 0, 0, NULL, yylineno, 0);
						} else {
							index = ST_insert($2, scopeName, varDeclNode->children[0]->nodeKind, 0, 0, NULL, yylineno, 0);
						}
						addChild(varDeclNode, maketreeWithVal(T_ID, index));
						addChild(varDeclNode, maketree(T_SEMICLN));
						$$ = varDeclNode;
					  }
					;
typeSpecifier		: KWD_INT
					  {
					    $$ = maketree(T_INTEGER);
					  }
					| KWD_CHAR
					  {
					    $$ = maketree(T_CHARACTER);
					  }
					| KWD_VOID
					  {
					    $$ = maketree(T_VOID);
					  }
					;
					
funDecl				: typeSpecifier ID LPAREN
					  {
						scopeLevel++;
						newSym.argsCount = 0;
						strcpy(scopeName, $2);
						funDeclNode = maketree(FUNDECL);
						addChild(funDeclNode, $1);
					  }
					  formalDeclList RPAREN
					  {
						int index = ST_insert($2, "global", T_FUNCTION, funDeclNode->children[0]->nodeKind, newSym.argsCount, newSym.args, yylineno, 0);
						addChild(funDeclNode, maketreeWithVal(T_ID, index));
						addChild(funDeclNode, maketree(T_LPAREN));
						addChild(funDeclNode, $5);
						addChild(funDeclNode, maketree(T_RPAREN));
					  }
					  funBody
					  {
						scopeLevel--;
					    addChild(funDeclNode, $8);
					    $$ = funDeclNode;
					  }
					| typeSpecifier ID LPAREN
					  {
						scopeLevel++;
						newSym.argsCount = 0;
						strcpy(scopeName, $2);
						funDeclNode = maketree(FUNDECL);
						addChild(funDeclNode, $1);
						int index = ST_insert($2, "global", T_FUNCTION, funDeclNode->children[0]->nodeKind, newSym.argsCount, newSym.args, yylineno, 0);
						addChild(funDeclNode, maketreeWithVal(T_ID, index));
						addChild(funDeclNode, maketree(T_LPAREN));
					  }
					  RPAREN funBody
					  {
						scopeLevel--;
						addChild(funDeclNode, maketree(T_RPAREN));
						addChild(funDeclNode, $6);
						$$ = funDeclNode;
					  }
					;
formalDeclList		: formalDecl
					  {
						tree *formalDeclListNode = maketree(FORMALDECLLIST);
						addChild(formalDeclListNode, $1);
						$$ = formalDeclListNode;
					  }
					| formalDecl COMMA formalDeclList
					  {
						tree *formalDeclListNode = maketree(FORMALDECLLIST);
						addChild(formalDeclListNode, $1);
						addChild(formalDeclListNode, maketree(T_COMMA));
						addChild(formalDeclListNode, $3);
						$$ = formalDeclListNode;
					  }
					;
formalDecl			: typeSpecifier ID
					  {
						tree *formalDeclNode = maketree(FORMALDECL);
						addChild(formalDeclNode, $1);
						int index = ST_insert($2, scopeName, formalDeclNode->children[0]->nodeKind, 0, 0, NULL, yylineno, 0);
						addChild(formalDeclNode, maketreeWithVal(T_ID, index));
						$$ = formalDeclNode;
						newSym.argsCount++;
						newSym.args[newSym.argsCount-1] = formalDeclNode->children[0]->nodeKind;
					  }
					| typeSpecifier ID LSQ_BRKT RSQ_BRKT
					  {
					    int type = 0;
						tree *formalDeclNode = maketree(FORMALDECL);
						addChild(formalDeclNode, $1);
						
						switch (formalDeclNode->children[0]->nodeKind) {
							case T_INTEGER:
									type = T_INTARRAY;
									break;
							case T_CHARACTER:
									type = T_CHARARRAY;
									break;
							case T_VOID:
									type = T_VOIDARRAY;
									break;
							default:
									type = 0;
						}

						int index = ST_insert($2, scopeName, type, 0, 0, NULL, yylineno, 0);
						addChild(formalDeclNode, maketreeWithVal(T_ID, index));
						addChild(formalDeclNode, maketree(T_LSQ_BRKT));
						addChild(formalDeclNode, maketree(T_RSQ_BRKT));
						$$ = formalDeclNode;
						newSym.argsCount++;
						newSym.args[newSym.argsCount-1] = type;
					  }
					;
funBody				: LCRLY_BRKT localDeclList statementList RCRLY_BRKT
					  {
						tree *funBodyNode = maketree(FUNBODY);
						addChild(funBodyNode, maketree(T_LCRLY_BRKT));
						addChild(funBodyNode, $2);
						addChild(funBodyNode, $3);
						addChild(funBodyNode, maketree(T_RCRLY_BRKT));
						$$ = funBodyNode;
					  }
					;
localDeclList		: 
					  {
						tree *localDeclListNode = maketreeWithVal(LOCALDECLLIST, -1);
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
statementList		: 
					  {
						tree *statementListNode = maketreeWithVal(STATEMENTLIST, -1);
						$$ = statementListNode;
					  }
					| statement statementList
					  {
						tree *statementListNode = maketree(STATEMENTLIST);
						addChild(statementListNode, $1);
						addChild(statementListNode, $2);
						$$ = statementListNode;
					  }
					;
statement			: compoundStmt
					  {
						tree *statementNode = maketree(STATEMENT);
						addChild(statementNode, $1);
						$$ = statementNode;
					  }
					| assignStmt
					  {
						tree *statementNode = maketree(STATEMENT);
						addChild(statementNode, $1);
						$$ = statementNode;
					  }
					| condStmt
					  {
						tree *statementNode = maketree(STATEMENT);
						addChild(statementNode, $1);
						$$ = statementNode;
					  }
					| loopStmt
					  {
						tree *statementNode = maketree(STATEMENT);
						addChild(statementNode, $1);
						$$ = statementNode;
					  }
					| returnStmt
					  {
						tree *statementNode = maketree(STATEMENT);
						addChild(statementNode, $1);
						$$ = statementNode;
					  }
					;
compoundStmt		: LCRLY_BRKT statementList RCRLY_BRKT
					  {
						tree *compoundStmtNode = maketree(COMPOUNDSTMT);
						addChild(compoundStmtNode, maketree(T_LCRLY_BRKT));
						addChild(compoundStmtNode, $2);
						addChild(compoundStmtNode, maketree(T_RCRLY_BRKT));
						$$ = compoundStmtNode;
					  }
					;
assignStmt			: var OPER_ASGN expression SEMICLN
					  {
						tree *assignStmtNode = maketree(ASSIGNSTMT);
						addChild(assignStmtNode, $1);
						addChild(assignStmtNode, maketree(T_OPER_ASGN));
						addChild(assignStmtNode, $3);
						addChild(assignStmtNode, maketree(T_SEMICLN));

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
						    if (symbolTable[tempExpr->val].type == T_FUNCTION) {
						      expType = symbolTable[tempExpr->val].returnType;
						    } else {
						      expType = symbolTable[tempExpr->val].type;
						    }
						  } else {  // if constant
						    expType = tempExpr->nodeKind;
						  }
					    
					      if (!((varType == T_INTEGER || varType == T_INTARRAY) && (expType == T_INTEGER || expType == T_INTCONST || expType == T_INTARRAY)) &&
							  !((varType == T_VOID || varType == T_VOIDARRAY) && (expType == T_VOID || expType == T_VOIDARRAY)) &&
							  !((varType == T_CHARACTER || varType == T_CHARARRAY) && (expType == T_CHARACTER || expType == T_CHARCONST || expType == T_CHARARRAY)) &&
							  !(varType == T_CHARARRAY && expType == T_STRCONST)) {
						    printf("Error: %d: Type mismatch in assignment\n", yylineno);
						  }
						}

					    
						$$ = assignStmtNode;
					  }
					| expression SEMICLN
					  {
						tree *assignStmtNode = maketree(ASSIGNSTMT);
						addChild(assignStmtNode, $1);
						addChild(assignStmtNode, maketree(T_SEMICLN));
						$$ = assignStmtNode;
					  }
					;
condStmt			: KWD_IF LPAREN expression RPAREN statement %prec LOWEST_PRECEDENCE
					  {
						tree *condStmtNode = maketree(CONDSTMT);
						addChild(condStmtNode, maketree(T_KWD_IF));
						addChild(condStmtNode, maketree(T_LPAREN));
						addChild(condStmtNode, $3);
						addChild(condStmtNode, maketree(T_RPAREN));
						addChild(condStmtNode, $5);
						$$ = condStmtNode;
					  }
					| KWD_IF LPAREN expression RPAREN statement KWD_ELSE statement
					  {
						tree *condStmtNode = maketree(CONDSTMT);
						addChild(condStmtNode, maketree(T_KWD_IF));
						addChild(condStmtNode, maketree(T_LPAREN));
						addChild(condStmtNode, $3);
						addChild(condStmtNode, maketree(T_RPAREN));
						addChild(condStmtNode, $5);
						addChild(condStmtNode, maketree(T_KWD_ELSE));
						addChild(condStmtNode, $7);
						$$ = condStmtNode;
					  }
					;
loopStmt			: KWD_WHILE LPAREN expression RPAREN statement
					  {
						tree *loopStmtNode = maketree(LOOPSTMT);
						addChild(loopStmtNode, maketree(T_KWD_WHILE));
						addChild(loopStmtNode, maketree(T_LPAREN));
						addChild(loopStmtNode, $3);
						addChild(loopStmtNode, maketree(T_RPAREN));
						addChild(loopStmtNode, $5);
						$$ = loopStmtNode;
					  }
					;
returnStmt			: KWD_RETURN SEMICLN
					  {
						tree *returnStmtNode = maketree(RETURNSTMT);
						addChild(returnStmtNode, maketree(T_KWD_RETURN));
						addChild(returnStmtNode, maketree(T_SEMICLN));
						$$ = returnStmtNode;
					  }
					| KWD_RETURN expression SEMICLN
					  {
						tree *returnStmtNode = maketree(RETURNSTMT);
						addChild(returnStmtNode, maketree(T_KWD_RETURN));
						addChild(returnStmtNode, $2);
						addChild(returnStmtNode, maketree(T_SEMICLN));
						$$ = returnStmtNode;
					  }
					;
var					: ID
					  {
						tree *varNode = maketree(VAR);
						addChild(varNode, maketreeWithVal(T_ID, ST_lookup($1, scopeName, T_UNKNOWN, yylineno)));
						$$ = varNode;
					  }
					| ID LSQ_BRKT addExpr RSQ_BRKT
					  {
						tree *varNode = maketree(VAR);
						int index = ST_lookup($1, scopeName, T_UNKNOWN, yylineno);
						addChild(varNode, maketreeWithVal(T_ID, index));
						addChild(varNode, maketree(T_LSQ_BRKT));
						addChild(varNode, $3);
						
						if (index >= 0) {
							tree *temp = varNode->children[2];
							while (temp->numChildren > 0) {
								temp = temp->children[0];
							}
							if (symbolTable[varNode->children[0]->val].type == T_INTEGER ||
								symbolTable[varNode->children[0]->val].type == T_CHARACTER ||
								symbolTable[varNode->children[0]->val].type == T_VOID) {
								printf("Error: %d: Indexing a scalar variable\n", yylineno);
							} else {
								if (temp->nodeKind == T_ID) {
								  if (symbolTable[temp->val].type == T_FUNCTION) {
									if (symbolTable[temp->val].returnType != T_INTEGER)
									  printf("Error: %d: Indexing an array variable with a non-integer type\n", yylineno);
								  } else {
									if (symbolTable[temp->val].type != T_INTEGER)
									  printf("Error: %d: Indexing an array variable with a non-integer type\n", yylineno);
								  }
								} else {  // if constant
								  if (temp->nodeKind != T_INTCONST) {
									printf("Error: %d: Indexing an array variable with a non-integer type\n", yylineno);
								  } else {
									int arrIndex = atoi(yylval.strval);
									if (arrIndex >= symbolTable[index].arrBound)
									  printf("Error: %d: Indexing an array with an out-of-bounds integer literal\n", yylineno);
								  }
								}
							}
						}
						
						addChild(varNode, maketree(T_RSQ_BRKT));
						$$ = varNode;
					  }
					;
expression			: addExpr
					  {
						tree *expressionNode = maketree(EXPRESSION);
						addChild(expressionNode, $1);
						$$ = expressionNode;
					  }
					| expression relop addExpr
					  {
						tree *expressionNode = maketree(EXPRESSION);
						addChild(expressionNode, $1);
						addChild(expressionNode, $2);
						addChild(expressionNode, $3);
						$$ = expressionNode;
					  }
					;
relop				: OPER_LTE
					  {
						tree *relopNode = maketree(RELOP);
						addChild(relopNode, maketree(T_OPER_LTE));
						$$ = relopNode;
					  }
					| OPER_LT
					  {
						tree *relopNode = maketree(RELOP);
						addChild(relopNode, maketree(T_OPER_LT));
						$$ = relopNode;
					  }
					| OPER_GT
					  {
						tree *relopNode = maketree(RELOP);
						addChild(relopNode, maketree(T_OPER_GT));
						$$ = relopNode;
					  }
					| OPER_GTE
					  {
						tree *relopNode = maketree(RELOP);
						addChild(relopNode, maketree(T_OPER_GTE));
						$$ = relopNode;
					  }
					| OPER_EQ
					  {
						tree *relopNode = maketree(RELOP);
						addChild(relopNode, maketree(T_OPER_EQ));
						$$ = relopNode;
					  }
					| OPER_NEQ
					  {
						tree *relopNode = maketree(RELOP);
						addChild(relopNode, maketree(T_OPER_NEQ));
						$$ = relopNode;
					  }
					;
addExpr				: term
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
addop				: OPER_ADD
					  {
						tree *addopNode = maketree(ADDOP);
						addChild(addopNode, maketree(T_OPER_ADD));
						$$ = addopNode;
					  }
					| OPER_SUB
					  {
						tree *addopNode = maketree(ADDOP);
						addChild(addopNode, maketree(T_OPER_SUB));
						$$ = addopNode;
					  }
					;
term				: factor
					  {
						tree *termNode = maketree(TERM);
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
mulop				: OPER_MUL
					  {
						tree *mulopNode = maketree(MULOP);
						addChild(mulopNode, maketree(T_OPER_MUL));
						$$ = mulopNode;
					  }
					| OPER_DIV
					  {
						tree *mulopNode = maketree(MULOP);
						addChild(mulopNode, maketree(T_OPER_DIV));
						$$ = mulopNode;
					  }
					;
factor				: LPAREN expression RPAREN
					  {
						tree *factorNode = maketree(FACTOR);
						addChild(factorNode, maketree(T_LPAREN));
						addChild(factorNode, $2);
						addChild(factorNode, maketree(T_RPAREN));
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
						tree *factorNode = maketree(FACTOR);
						addChild(factorNode, maketreeWithVal(T_INTCONST, atoi($1)));
						$$ = factorNode;
					  }
					| CHARCONST
					  {
						tree *factorNode = maketree(FACTOR);
						addChild(factorNode, maketreeWithVal(T_CHARCONST, $1));
						$$ = factorNode;
					  }
					| STRCONST
					  {
						tree *factorNode = maketree(FACTOR);
						int index = ST_insert($1, "", STRCONST, 0, 0, NULL, yylineno, 0);
						addChild(factorNode, maketreeWithVal(T_STRCONST, index));
						$$ = factorNode;
					  }
					;
funcCallExpr		: ID LPAREN
					  {
					    funcCallIndex++;
						tree *funcCallExprNode = maketree(FUNCCALLEXPR);
						funcCallAddr[funcCallIndex] = funcCallExprNode;
						int index = ST_lookup($1, "global", T_FUNCTION, yylineno);
						addChild(funcCallAddr[funcCallIndex], maketreeWithVal(T_ID, index));
						addChild(funcCallAddr[funcCallIndex], maketree(T_LPAREN));
						funcCallArr[funcCallIndex] = index;
					  }
					   argList RPAREN
					  {
						addChild(funcCallAddr[funcCallIndex], $4);
						addChild(funcCallAddr[funcCallIndex], maketree(T_RPAREN));
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
						      if (!((origType == T_INTEGER || origType == T_INTARRAY) && (callType == T_INTEGER || callType == T_INTCONST || callType == T_INTARRAY)) &&
							      !((origType == T_VOID || origType == T_VOIDARRAY) && (callType == T_VOID || callType == T_VOIDARRAY)) &&
							      !((origType == T_CHARACTER || origType == T_CHARARRAY) && (callType == T_CHARACTER || callType == T_CHARCONST || callType == T_CHARARRAY)) &&
							      !(origType == T_CHARARRAY && callType == T_STRCONST)) {
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
						int index = ST_lookup($1, "global", T_FUNCTION, yylineno);
						addChild(funcCallExprNode, maketreeWithVal(T_ID, index));
						addChild(funcCallExprNode, maketree(T_LPAREN));
						addChild(funcCallExprNode, maketree(T_RPAREN));
						$$ = funcCallExprNode;
						
						if (symbolTable[index].argsCount > 0) {
						    printf("Error: %d: Function signature mismatch: too few parameters\n", yylineno);
						}
					  }
					;
argList				: expression
					  {
						tree *argListNode = maketree(ARGLIST);
						addChild(argListNode, $1);
						
						tree *temp = argListNode->children[0];
						while (temp->numChildren > 0) {
							temp = temp->children[0];
						}
						
						int tempType = -1;
						if (temp->nodeKind == T_ID) {
						  if (symbolTable[temp->val].type == T_FUNCTION) {
						    tempType = symbolTable[temp->val].returnType;
						  } else {
						    tempType = symbolTable[temp->val].type;
						  }
						} else {  // if constant
						  tempType = temp->nodeKind;
						}
						funcCallStack = push(funcCallStack, temp->val, tempType, funcCallIndex);

						$$ = argListNode;
					  }
					| argList COMMA expression
					  {
						tree *argListNode = maketree(ARGLIST);
						addChild(argListNode, $1);
						addChild(argListNode, maketree(T_COMMA));
						addChild(argListNode, $3);
						
						tree *temp = argListNode->children[2];
						while (temp->numChildren > 0) {
							temp = temp->children[0];
						}
						
						int tempType = -1;
						if (temp->nodeKind == T_ID) {
						  if (symbolTable[temp->val].type == T_FUNCTION) {
						    tempType = symbolTable[temp->val].returnType;
						  } else {
						    tempType = symbolTable[temp->val].type;
						  }
						} else {  // if constant
						  tempType = temp->nodeKind;
						}
						funcCallStack = push(funcCallStack, temp->val, tempType, funcCallIndex);

						$$ = argListNode;
					  }
					;

%%

int yyerror(char * msg) {
	printf("Error: %d: %s\n", yylineno, msg);
	return 0;
}