/* Copyright 1995, Lawrence Berkeley Laboratory */

/* testsort.c -  */

/*
modification history
--------------------
15mar95,whg  written.
*/
/*
DESCRIPTION
*/
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#include "dstype.h"
/****************************************************************************
*
*
*/
int testCmpStr(char *s1, char *s2, char *pn)
{
	int c, n;
	
	n = *(int *)pn;
	c = strncmp(s1, s2, n);
	
	return c == 0 ? 0 : c > 0 ? 1 : -1;
}
/****************************************************************************
*
*
*/
int testCmpInt(char *n1, char *n2, char *dum)
{
	if (dum != NULL) {
		return -2;
	}
	return *(int *)n1 > *(int *)n2 ? 1 : *(int *)n1 == *(int *)n2 ? 0 : -1;
} 
/****************************************************************************
*
*
*/ 
int testSort(void)
{
	char list[][4] = {"jan", "feb", "mar", "apr", "may", "jun",
					"jul", "aug", "sep", "oct", "nov", "dec"}, *ptr;
	char slist[][4] = {"apr", "aug", "dec", "feb", "jan", "jul",
					"jun", "mar", "may", "nov", "oct", "sep"};
	int a[20], i, j, n = 12, size = 4, v;
	
	for (i = 0; i < 20; i++) {
		for (j = 0; j < i; j++) {
			a[j] = rand();
		}
		v = a[0];
		if (!dsQuickSort((char *)a, j, sizeof(int), testCmpInt, 0)) {
			printf("sort for int FAILED\n");
			return FALSE;
		}
		for (j = 1; j < i; j++) {
			if (a[j-1] > a[j]) {
				printf("int check FAILED\n");
				return 0;
			}
		}
		if ((i > 0 ? 1 : 0 ) != dsBinSearch(&ptr, (char *)&v, (char *)a,
			 i, sizeof(int), testCmpInt, 0)) {
			 printf("dsBinSearch failed\n");
			 return FALSE;
		}
		if ((i == 0 && ptr != NULL) || 
			(i > 0 && (ptr == NULL || v != *(int *)ptr))) {
			printf("wrong value for dsBinSearch\n");
			return FALSE;
		}
	}	
	if (!dsQuickSort((char *)list, n, size, testCmpStr, (char *)&size)) {
		printf("sort for strings FAILED\n");
		return FALSE;
	}
	for (i = 0; i < 12; i++) {
		printf("%s ", list[i]);
		if (strcmp(list[i], slist[i]) != 0) {
			printf("\nsort compare failed\n");
			return FALSE;
		}
	}
	if (!dsBinSearch(&ptr, "dec", (char *)list, n, size,
		testCmpStr, (char *)&size)) {
		printf("\ndec not found\n");
		return 0;
	}		  
	printf("\nsearch for dec found %s\n", ptr);
	if (dsBinSearch(&ptr, "jim", (char *)list, n, size,
		testCmpStr, (char *)&size)) {
		printf("jim found\n");
		return 0;
	}
	if (ptr == NULL) {
		printf("search for jim returned NULL\n");
		return 0;
	}
	printf("search for jim false, returned %s\n", ptr);
	return 1;
}
