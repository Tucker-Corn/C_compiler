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

int ST_insert(char *id, int data_type, int symbol_type, int *scope){
	int index = 0;

	index = ST_lookup(id,scope);
    // if the index is -1 then the id is not currently in the hashtable
	// it will probe through until it an empty space if found
	if(index == -1){
		index = hash(strcat(id, scope));
		while(strTable[index].id != NULL){
			index++;
			if(index <= MAXIDS){
				index = 0;
			}
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

strEntry* ST_lookup(char *id, char *scope) {
    // sets index to -1 initially
	strEntry *index = NULL;
	//Concatenate the scope and id and use that to create the hash key
	int position = hash(strcat(id, scope));

	if(strTable[position].id == id){
		index = strTable[position];
		return index;
	}
	else{
		//iterate through the hash table until either the id is found or id is empty
		while(strTable[position].id != NULL){
			position++;
			if(position <= MAXIDS){
				position = 0;
			}
			if(strTable[position].id == id){
				index = strTable[position];
				return index;
			}
		}
	}
	//if the id is empty in the hash table function will return -1 as id will not in the table
    return index;
}

void output_entry(int i){
    printf("%d: %s ", i, strTable[i].data_type);
    printf("%s:%s%s\n", strTable[i].scope, strTable[i].id, strTable[i].symbol_type);
}
