#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <tree.h>   // definition for nodes of the AST
#include <strtab.h> // the symbol table for needed information about nodes

// a counter for jump labels to be made unique
int labelcounter=0;
// reset register counter (done after calling a func or at beginning of func)
int registercounter=7;

// defines to get the next label counter or to reset registers
#define nextlabel() (++labelcounter)
#define resetregisters() (registercounter=7)

/*
An example of how / why for label counters

Suppose we have:
FORNODE
FORNODE->children[0] = stmtnode (init)
FORNODE->children[1] = condnode (condition)    
FORNODE->children[2] = compoundstmt (body)
FORNODE->children[3] = stmtnode (post)
*/

void emitforstmt(tree *ast,FILE *outfile) {
  // Get a unique label number for the labels of this FORSTMT
  const int forlabel = nextlabel();

  // emit the for statement:
  fprintf(outfile,"FORSTMT_INIT%d: # Preparing to do for loop\n",
                  forlabel);
  gencode(ast->children[0],outfile);
  fprintf(outfile,"FORSTMT_COND%d: # For statement condition\n",
                  forlabel);
  // how you evaluate conditions will probably vary
  const int result = gencode(ast->children[1],outfile);
  // here I am assuming this register will equal zero if the condition is false
  fprintf(outfile,"beq %s, $zero, FORSTMT_END%d # Exit the for statement\n",
                  registernames[result], forlabel);
  // otherwise we do the body of the for loop
  fprintf(outfile,"FORSTMT_BODY%d: # Begin the for statement\'s body\n",
                  forlabel);
  // emit the body of the loop
  gencode(ast->children[2],outfile);
  // do the post expression
  fprintf(outfile,"FORSTMT_POST%d: # For statement post statement\n",
                  forlabel);
  // We need to return to the for loop's condition
  fprintf(outfile,"j FORSTMT_COND%d # Return to for condition\n",
                  forlabel);
  // Now we print the label for the for statement exit
  fprintf(outfile,"FORSTMT_END%d: # End of for loop\n",
                  forlabel);
}

emitOperation(int op, int dst, int r1){
	switch(op){
		case:
	}

}


Expression(tree *ast){
	int result;
	int reg1, reg2;
	switch(ast->nodekind){
		case * , / , + , - :

			reg1 = Expression(getFirstChild(ast));
			reg2 = Expression(getSecondChild(ast));
			result = nextreg();
			emitOperation(ast->nodekind, reg1, reg2, result);
			break;
		case ID:
			reg1 = base(ast);
			reg2 = offset(ast);
			result = nextreg();
			emitOperation(ast->nodekind, reg1, reg2, result);
			break;
		case NUM:
			result = nextreg();
			emitOperation(ast->nodekind, ast->val, 0, result);

	}
	return result;
}

int gencode(tree *ast,FILE *outfile) {
  // variables can't be defined inside a switch
  // move more complicated things to their own function to keep this cleaner
  // (e.g., function definition needs to setup/teardown stack, ...)
  int lhs,rhs,dst;
  switch(ast->nodeKind) {
// output(6);
    case INTCONST:
      dst = getregister();
      fprintf(outfile,"li %s, %d\n",
                      registernames[dst], ast->value);
      return dst;
/*
 *     FUNCCALLNODE
 *     ARGLIST
 *     EXPRESSION , ....
 */
    case FUNCDEFNODE:
      setupframe();
      resetregisters();
      for all statements in funcdefnode emitcode();
      teardownframe_andrestore();
      setreturnvalue();
      if (strcmp(funcname,"main")==0)
        syscall_for_clean_exit();
      else {
        fprintf(outfile,"jr $ra\n");
      }
    case FUNCCALLNODE:
      if (strcmp(ast->id,"output")) {
        rhs = gencode(ast->children[0]->children[0]);
        printf("li $v0, 1\n");
        printf("mov $a0, %s\n",registernames[rhs]);
        printf("syscall\n");
      }
      else {
        // save registers, prepare to move context
        // resolve arguments, put them where appropriate
        // jump and link to function label
        // immediately after jump and link all $t_ registers are garbage.
        // return value will be (somewhere)
        resetregisters();
        // anything needed will have to be restored from the stack
      }
      // return 0, break, (?)
      return 0;
      break;
    default: break;
  }
  if (nodeKind == assignStmt){           //assignments
    t1 = Expression(ast->getChild[1])

    t2 = base(ast->getChild[0])
    t3 = offset(ast->getChild[0])

    emitOperation(st,t1,t2,t3)
  }


  if(nodeKind == condStmt){           //conditionals
    t1 = Expression(getCondNode(ast->node))

    t1 = Expression(getCondNode(ast->node));   
    t2 = nextReg(); 
    emitOperation(ldi, 1, t2);
    l1 = newLabel();
    emitOperation(beq, t2, t1, l1);   

    t3 = Expression(getElseClause(ast->node));  
    l2 = newLabel(); 
    emitOperation(jump, l2);
    emitOperation(label, l1)                  
    t4 = Expression(getThenClause(ast->node));  
    emitOperation(label, l2) 
  }

  return -1; // (?)
}

int getCondNode(ast->nodeKind){

}
int getElseClause(ast->nodeKind){

}
int getThenClause(ast->nodeKind){

}
/*
Number  | Name      | Description
0       | $zero     | The value 0
2-3     | $v0 - $v1 | (values) from expression evaluation and function results
4-7     | $a0 - $a3 | (arguments) First four parameters for subroutine
8-15    | $t0 - $t7 | Temporary variables
16-23   | $s0 - $s7 | Saved values representing final computed results
24-25   | $t8 - $t9 | Temporary variables2
31      | $ra       |Return address
*/
char *registernames[] = { "$zero","$at","$v0","$v1","$a0","$a1","$a2","$a3",
                          "$t0","$t1","$t2","$t3","$t4","$t5","$t6","$t7",
                          "$s0","$s1","$s2","$s3","$s4","$s5","$s6","$s7",
                          "$t8","$t9","$k0","$k1","$gp","$sp","$fp","$ra" };
// get the next available register
// this naiively just cycles through $t0 -> $t9 -> $t0
int nextreg() {
  // jump the gap from $t7 to $t8
  if (++registercounter == 16) registercounter = 24;
  // wrap from $t9 to $t0
  else if (registercounter == 26) registercounter = 8;
  return registercounter;
}
