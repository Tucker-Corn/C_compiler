#include<tree.h>
#include<strtab.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

char *nodeNames[8] = {"program", "declList", "decl", "varDecl", "typeSpecifier", "funDecl", "formalDeclList", "formalDecl", "funBody", "localDeclList", "statementList", "statement",
 "compoundStmt", "assignStmt", "condStmt", "loopStmt", "returnStmt", "var", "expression", "relop", "addExpr", "addop", "term", "mulop", "factor", "funcCallExpr", "argList"};
tree *ast;

tree *maketree(int kind){
  tree *this = (tree *) malloc(sizeof(struct treenode));
  this->nodeKind = kind;
  this->numChildren = 0;
  return this;

}

tree *maketreeWithVal(int kind, int val){
  
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

void printAst(tree *root, int nestLevel){
  if (node != NULL) { //check if null in all tree functions
    if (node->nodeKind == INT_TYPE) printf("<%d>\n", node->val);
    else if (node->nodeKind < 32) printf("%s\n", nodeNames[node->nodeKind]);
    else printf("nodeKind = %d ? val = %d ?\n", node->nodeKind, node->val);
  }
   if (root->nodeKind == INT_TYPE) {
	printf("<%d>\n", root->val);
    }    
    else {
	printf("%s\n", nodeNames[root->nodeKind]);
    }

  int i, j;

  for (i = 0; i < root->numChildren; i++)  {
    for (j = 0; j < nestLevel; j++) 
      printf("    ");
    printAst(getChild(node, i), nestLevel + 1);
  }

}
