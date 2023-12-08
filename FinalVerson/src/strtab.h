#ifndef STRTAB_H
#define STRTAB_H
#define MAXIDS 1000

enum dataType {INT_TYPE, CHAR_TYPE, VOID_TYPE};
enum symbolType {SCALAR, ARRAY, FUNCTION};

typedef struct param{
    int data_type;
    int symbol_type;
    struct param* next;
} param;

typedef struct strEntry{
    char* id;
    char* scope;
    int   data_type;
    int   symbol_type;
    int   size; //Num elements if array, num params if function
    param*  params;
} symEntry;

/* You should use a linear linklist to keep track of all parameters passed to a function. The working_list_head should point to the beginning of the linklist and working_list_end should point to the end. Whenever a parameter is passed to a function, that node should also be added in this list. */
param *working_list_head = NULL;
param *working_list_end = NULL;

typedef struct table_node{
    symEntry* strTable[MAXIDS];
    int numChildren;
    struct table_node* parent;
    struct table_node* first_child; // First subscope
    struct table_node* second_child;
    struct table_node* third_child;
    struct table_node* last_child;  // Most recently added subscope
    struct table_node* next; // Next subscope that shares the same parent
} table_node; // Describes each node in the symbol table tree and is used to implement a tree for the nested scope as discussed in lecture 13 and 14.

table_node* current_scope = NULL; // A global variable that should point to the symbol table node in the scope tree as discussed in lecture 13 and 14.



/* Inserts a symbol into the current symbol table tree. Please note that this function is used to instead into the tree of symbol tables and NOT the AST. Start at the returned hash and probe until we find an empty slot or the id.  */
int ST_insert(char *id, int data_type, int symbol_type, int* scope);

/* The function for looking up if a symbol exists in the current_scope. Always start looking for the symbol from the node that is being pointed to by the current_scope variable*/
symEntry* ST_lookup(char *id);

// Creates a new scope within the current scope and sets that as the current scope.
void new_scope();

// Moves towards the root of the sym table tree.
void up_scope();

#endif
