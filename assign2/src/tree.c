#include<tree.h>
#include<strtab.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

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

   if (root->nodeKind == INTEGER) {
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
