#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "strtab.h"


/* Provided is a hash function that you may call to get an integer back. */
unsigned long hash(unsigned char *str)
{
    unsigned long hash = 5381;
    int c;

    while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

int ST_insert(char *id, char *scope, int data_type, int symbol_type){
	int index = 0;
    //Concatenate the scope and id and use that to create the hash key
	unsigned char *key = sprintf(id, scope);
	index = ST_lookup(id,scope);
    // TODO: Use ST_lookup to check if the id is already in the symbol table. If yes, ST_lookup will return an index that is not -1. if index != -1, that means the variable is already in the hashtable. Hence, no need to insert that variable again. However, if index == -1, then use linear probing to find an empty spot and insert there. Then return that index.
	if(index == -1){
		index = hash(key);
		while(strTable[index].id != NULL){
			index++;
		}
		//place new entry into the empty spot
		strTable[index].id = id;
		strTable[index].scope = scope;
		strTable[index].data_type = data_type;
		strTable[index].symbol_type = symbol_type;

	}
	else{
		// id is already there so do nothing
	}
    return index;
}

int ST_lookup(char *id, char *scope) {
    // sets index to -1 initially
	int index = -1;
	//Concatenate the scope and id and use that to create the hash key
	unsigned char *key = sprintf(id, scope);
	int position = hash(key);

	if(strTable[position].id == id){
		index = position;
		return index;
	}
	else{
		//iterate through the hash table until either the id is found or id is empty
		while(strTable[position].id != NULL){
			position++;
			if(strTable[position].id == id){
				index = position;
				return index;
			}
		}
	}
	//if the id is empty in the hash table function will return -1 as id will not in the table
    return index;
}

void output_entry(int i){
    printf("%d: %s ", i, types[strTable[i].data_type]);
    printf("%s:%s%s\n", strTable[i].scope, strTable[i].id, symTypeMod[strTable[i].symbol_type]);
}

