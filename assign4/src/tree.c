#include <tree.h>
#include <table.h>
#include <stdio.h>
#include <stdlib.h>

/* string values for ast node types, makes tree output more readable */
char *nodeNames[70] = {"program", "declList", "decl", "varDecl", "localVarDecl", "typeSpecifier",
                      "funDecl", "formalDeclList", "formalDecl", "funBody",
                      "localDeclList", "statementList", "statement", "compoundStmt",
                      "assignStmt", "condStmt", "loopStmt", "returnStmt", "var",
                      "expression", "relop", "addExpr", "addop", "term", "mulop",
                      "factor", "funcCallExpr", "argList", "if", "else",
                      "while", "return", "+", "-", "*", "/", "<", ">", ">=", "<=", "==", "!=",
                      "=", "[", "]", "{", "}", "(", ")", ",", ";", "intconst",
                      "charconst", "strconst", "identifier", "error", "illegal token",
                      "unknown", "int", "void", "char", "string", "function", "int[]", "void[]", "char[]"};

tree *maketree(int kind) {
  tree *this = (tree *) malloc(sizeof(struct treenode));
  this->nodeKind = kind;
  this->numChildren = 0;
  return this;

}

tree *maketreeWithVal(int kind, int val) {
  tree *this = (tree *) malloc(sizeof(struct treenode));
  this->nodeKind = kind;
  this->numChildren = 0;
  this->val = val;
  return this;

}

void addChild(tree *parent, tree *child) {
  if (parent->numChildren == MAXCHILDREN) {
    printf("Cannot add child to parent node\n");
    exit(1);
  }
  nextAvailChild(parent) = child;
  parent->numChildren++;
}

void printAst(tree *node, int nestLevel) {
	switch (node->nodeKind) {
		case T_INTCONST:
			printf("<integer: %d>\n", node->val);
			break;
		case T_CHARCONST:
			printf("<character: '%c'>\n", node->val);
			break;
		case T_STRCONST:
			printf("<string: \"%s\">\n", symbolTable[node->val].name);
			break;
		case T_ID:
			if (node->val == -1) {
				printf("<Undeclared var/fun>\n");
			} else {
				if (symbolTable[node->val].type == T_FUNCTION) 
				  printf("<identifier: \"%s\", %s, %s>\n", symbolTable[node->val].name, nodeNames[symbolTable[node->val].returnType], symbolTable[node->val].scope);
				else
				  printf("<identifier: \"%s\", %s, %s>\n", symbolTable[node->val].name, nodeNames[symbolTable[node->val].type], symbolTable[node->val].scope);
			}
			break;
		default:
			if (node->numChildren == 0 && node->val != -1) {
				printf("<%s>\n", nodeNames[node->nodeKind]);
			} else {
				printf("%s\n", nodeNames[node->nodeKind]);
			}
	}

	int i, j;

	for (i = 0; i < node->numChildren; i++)  {
		for (j = 0; j < nestLevel; j++) 
			printf("    ");
		printAst(getChild(node, i), nestLevel + 1);
	}
}

void printAst2(tree *node, int nestLevel) {
	
    int i, j;
    printf ("%s\t%d\n", nodeNames[node->nodeKind], node->val);
    for (i = 0; i < node->numChildren; i++) 
    {
        for (j = 0; j < (nestLevel - 1); j++)
            printf ("| ");
        if (nestLevel > 0)
            printf ("+ ");
        printAst2 (getChild (node, i), nestLevel + 1);
    }
}

void printCode(tree *node, int nestLevel) {
	
	int i, j;

	switch (node->nodeKind) {
		case T_INTCONST:
			printf("%d", node->val);
			break;
		case T_CHARCONST:
			printf("'%c'", node->val);
			break;
		case T_STRCONST:
			printf("\"%s\"", symbolTable[node->val].name);
			break;
		case T_ID:
			if (node->val == -1) {
				printf("<unknown> ");
			} else {
				printf("%s ", symbolTable[node->val].name);
			}
			break;
		case T_LPAREN:
			printf("%s", nodeNames[node->nodeKind]);
			break;
		case T_LSQ_BRKT:
			printf("%s", nodeNames[node->nodeKind]);
			break;
		case T_LCRLY_BRKT:
			printf("%s\n", nodeNames[node->nodeKind]);
			break;
		case T_RCRLY_BRKT:
			printf("%s\n\n", nodeNames[node->nodeKind]);
			break;
		case T_SEMICLN:
			printf("%s\n", nodeNames[node->nodeKind]);
			break;
		default:
			if (node->numChildren == 0 && node->val != -1) {
				printf("%s ", nodeNames[node->nodeKind]);
			}	
		}

	for (i = 0; i < node->numChildren; i++)  {
		printCode(getChild(node, i), nestLevel);
	}
}

