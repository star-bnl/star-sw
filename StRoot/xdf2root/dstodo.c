/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dstodo.c - to do list for dataset lib*/

/*
modification history
--------------------
23feb95,whg  written.
*/
/*
DESCRIPTION
file to keep and print to do list
*/
#include <stdio.h>
/*****************************************************************************
*
*
*/
void dsToDo(void)
{
	char *list[] = {
		"* Allow path in dsFindEntry??? ",
		NULL};
	int i;
		
	if (list[0]) {
		printf("\nTO DO for datasets:\n");
		for(i = 0; list[i] != NULL; i++) {
			printf("  %s\n", list[i]);
		}
		printf("\n");
	}
}
		
