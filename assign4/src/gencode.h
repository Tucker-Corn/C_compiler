#ifndef GENCODE_H
#define GENCODE_H


int Expression(tree *ast);
int gencode(tree *ast,FILE *outfile);
void emitforstmt(tree *ast,FILE *outfile);
int getCondNode(ast->nodeKind);
int getElseClause(ast->nodeKind);
int getThenClause(ast->nodeKind);
int nextreg();
