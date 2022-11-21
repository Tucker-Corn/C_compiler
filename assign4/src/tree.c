#include<tree.h>
#include<strtab.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

/* string values for ast node types, makes tree output more readable */
char *nodeNames[33] = {"program", "declList", "decl", "varDecl", "typeSpecifier",
                       "funDecl", "formalDeclList", "formalDecl", "funBody",
                       "localDeclList", "statementList", "statement", "compoundStmt",
                       "assignStmt", "condStmt", "loopStmt", "returnStmt","expression",
                       "relop", "addExpr", "addop", "term", "mulop", "factor",
                       "funcCallExpr", "argList", "integer", "identifier", "var",
                       "arrayDecl", "char", "funcTypeName"};

char *typeNames[3] = {"int", "char", "void"};
char *ops[10] = {"+", "-", "*", "/", "<", "<=", "==", ">=", ">", "!="};

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

tree * maketreeWithNameVal(int kind, char *name)
{
    tree *this = (tree *) malloc(sizeof(struct treenode));
    this->nodeKind = kind;
    this->numChildren = 0;
    this->name = name;

    return this;
}

tree * maketreeWithChild(int kind, tree *child)
{
    tree *this = (tree *) malloc(sizeof(struct treenode));
    this->nodeKind = kind;
    addChild(this, child);
    this->numChildren++; /* added this increment during codegen */
    return this;
}


void addChild(tree *parent, tree *child)
{
      if (parent->numChildren == MAXCHILDREN)
      {
          printf("Cannot add child to parent node\n");
          exit(1);
      }
    if(child)
    {
        nextAvailChild(parent) = child;
        parent->numChildren++;
        child->parent = parent;
    }
}

tree * ChildByKind(tree *node, char *kind)
{
    tree * localNode = NULL;
    int i;
    for (i = 0; i < node->numChildren; i++)
    {
        if (strcmp(kind, nodeNames[node->children[i]->nodeKind]) == 0)
        {
            localNode = node->children[i];
        }
    }
    return localNode;
}

tree * NestedChildByKind(tree *node, char *kind, int nestLevel)
{
    tree * localNode = NULL;
    if (strcmp(nodeNames[node->nodeKind], kind) == 0)
    {
        return node;
    }
    int i;
    for (i = 0; i < node->numChildren && localNode == NULL; i++)
    {
        localNode = NestedChildByKind(getChild(node, i), kind, nestLevel + 1);
    }
    return localNode;
}


int isNodeName(tree * node, char *kind)
{
    int isKind = strcmp(getNodeName(node), kind);
    return isKind;
}

/*recursively count a kind of node in a given tree. */
int walkSubTree(tree *node, char* kind, int nestLevel)
{
    int count = 0;
    if (strcmp(nodeNames[node->nodeKind], kind) == 0)
    {
        count++;
    }
    int i;
    for (i = 0; i < node->numChildren; i++)
    { count += walkSubTree(getChild(node, i), kind, nestLevel + 1); }
    return count;
}
void printAst(tree *node, int nestLevel) {
      char* nodeName = nodeNames[node->nodeKind];
      if(strcmp(nodeName,"identifier") == 0){
          if(node->val == -1)
              printf("%s,%s\n", nodeName,"undeclared variable");
          else
              printf("%s,%s\n", nodeName,get_symbol_id(node->val));
      }
      else if(strcmp(nodeName,"integer") == 0){
          printf("%s,%d\n", nodeName,node->val);
      }
      else if(strcmp(nodeName,"char") == 0){
          printf("%s,%c\n", nodeName,node->val);
      }
      else if(strcmp(nodeName,"typeSpecifier") == 0){
          printf("%s,%s\n", nodeName,typeNames[node->val]);
      }
      else if(strcmp(nodeName,"relop") == 0 || strcmp(nodeName,"mulop") == 0 || strcmp(nodeName,"addop") == 0){
          printf("%s,%s\n", nodeName,ops[node->val]);
      }
      else{
          printf("%s\n", nodeName);
      }

      int i, j;

      for (i = 0; i < node->numChildren; i++)  {
          for (j = 0; j < nestLevel; j++)
              printf("    ");
          printAst(getChild(node, i), nestLevel + 1);
      }

}

void flattenList(tree *list, tree *subList){
    for(int i=0; i < subList->numChildren; i++){
        addChild(list,getChild(subList,i));
    }
}

