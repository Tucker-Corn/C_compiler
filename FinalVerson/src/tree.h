#ifndef TREE_H
#define TREE_H

#define MAXCHILDREN 100

typedef struct treenode tree;

/* tree node - you may want to add more fields */
struct treenode
        {
      int nodeKind;
      int numChildren;
      int val;
      char* name;
      int scope; // Used for var/id. Index of the scope. This works b/c only global and local.
      int type;
      int sym_type; // Only used by var to distinguish SCALAR vs ARRAY
      tree *parent;
      tree *children[MAXCHILDREN];
};

tree *ast; /* pointer to AST root */

/* builds sub tree with zeor children  */
tree *maketree(int kind);

/* builds sub tree with leaf node */
tree *maketreeWithVal(int kind, int val);

void addChild(tree *parent, tree *child);

void printAst(tree *root, int nestLevel);

/* Adds all children of sublist to list */
void flattenList(tree *list, tree *subList);

/* builds sub tree with leaf node and name */
tree * maketreeWithNameVal(int kind, char *name);

tree * maketreeWithChild(int kind, tree *child);

/* adds a child to the parent tree */
void addChild(tree *parent, tree *child);

tree * ChildByKind(tree *node, char *kind);

tree * NestedChildByKind(tree *node, char *kind, int nestLevel);

/* walk tree, return number of children of specified kind */
int countChildren(tree *node, char *kind);

int walkSubTree(tree *node, char* kind, int nestLevel);

/* prints the AST */
void printAst(tree *root, int nestLevel);

int isNodeName(tree * node, char *kind);


/* tree manipulation macros */
/* if you are writing your compiler in C, you would want to have a large collection of these */

#define nextAvailChild(node) node->children[node->numChildren]
#define getChild(node, index) node->children[index]
#define getFirstChild(node) node->children[0]
#define getSecondChild(node) node->children[1]
#define getThirdChild(node) node->children[2]
#define getNodeName(node) nodeNames[node->nodeKind]
#endif
