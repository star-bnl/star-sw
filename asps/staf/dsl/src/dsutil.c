/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsutil.c - data structure utility routines */

/*
modification history
--------------------
01a,24apr93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#define DS_GLOBAL_ONE
#include "dscodes.h"
#include "dstype.h"

static int dsAllocCalls = 0;
static int dsFreeCalls = 0;
static int dsTidSize = 0;
/******************************************************************************
*
* dsCheckDuplicate - verify dataset or type names are unique 
*
*/
int dsCheckDuplicate(char *name, size_t count, size_t stride)
{
	char *n1, *n2;
	size_t i;
	
	for (i = 1, n2 = name; i < count; i++) {
		n2 += stride;
		for (n1 = name; n1 < n2; n1 += stride) {
			if (strcmp (n1, n2) == 0) {
				DS_ERROR(DS_E_DUPLICATE_NAME);
			}
		}
	}
	return TRUE;
}
/******************************************************************************
*
* dsDsetAlloc - allocate memory for dataset or table
*
*/
char *dsDsetAlloc(size_t size)
{
	char *ptr;

	if ((ptr = malloc(size)) != NULL) {
		dsAllocCalls++;
	}
	else {
		DS_LOG_ERROR(DS_E_MALLOC_FAILURE);
	}
	return ptr;
}
/******************************************************************************
*
* dsDsetFree - free memory for dataset or table
*
*/
void dsDsetFree(void *ptr)
{
	if (ptr != NULL) {
		free((char *)ptr);
		dsFreeCalls++;
	}
}
/******************************************************************************
*
* dsDatasetAllocStats - print dataset allocation stats
*
*/
int dsDatasetAllocStats(void)
{
	printf("datasetAllocStats: allocCalls %d, freeCalls %d, diff %d\n",
		dsAllocCalls, dsFreeCalls, dsAllocCalls - dsFreeCalls);
	return TRUE;
}
/******************************************************************************
*
* dsIdentifier - get standard C identifier
*
*/
int dsIdentifier(char *dst, char *str, char **ptr)
{
	return dsNCopyName(dst, str, ptr, DS_NAME_DIM);
}
/******************************************************************************
*
* dsIsName - test next token for C name
*
*/
int dsIsName(char *str)
{
	for (;isspace(*str); str++);
	return (isalpha(*str) || *str == '_');
}
/******************************************************************************
*
* dsNameToMem - copy C name to memory
*
*/
int dsNameToMem(DS_MEM_T *mem, char *str)
{
	char *ptr;

	if (!dsNCopyName(mem->next, str, &ptr, mem->limit - mem->next)) {
		return FALSE;
	}
	mem->next += ptr - str;
	return TRUE;
}
/******************************************************************************
*
* dsIndent - put n tabs in mem
*
*/
int dsIndent(DS_MEM_T *mem, int n)
{
	while(n-- > 0) {
		if (!dsStrToMem(mem, "\t")) {
			return FALSE;
		}
	}
	return TRUE;
}
/******************************************************************************
*
* dsMapDef - first pass for parse of dataset and type declarations
*
*/
int dsMapDef(pSepCount, map, pMapCount, maxMapCount, str)
int *pSepCount;
int *map;
int *pMapCount;
int maxMapCount;
char *str;
{
	int mapCount = 0, nest = -1, sepCount = 0, stack[DS_MAX_NEST];

	for (; isalnum(*str) || isspace(*str) || *str == '_'; str++);

	if (*str != '{') {
		*pMapCount = mapCount;
		*pSepCount = 0;
		return TRUE;
	}
	for (;;) {
		for (; isalnum(*str) || isspace(*str) || *str == '_'; str++);
		switch (*str++) {

			case ';':
			case ',':
				map[stack[nest]]++;
				sepCount++;
				break;

			case '{':
				if (++nest >= DS_MAX_NEST) {
					DS_ERROR(DS_E_NESTED_TOO_DEEP);
				}
				stack[nest] = mapCount++;
				if (mapCount > maxMapCount) {
					DS_ERROR(DS_E_TOO_MUCH_NESTING);
				}
				map[stack[nest]] = 0;
				break;

			case '}':
				if (nest == 0) {
					*pMapCount = mapCount;
					*pSepCount = sepCount;
					return TRUE;
				}
				nest--;
				break;

			case '(':
				while(*str != '\0' && *str++ != ')');
				break;

			case '[':
				while(*str != '\0' && *str++ != ']');
				break;

			default: 
				DS_ERROR(DS_E_SYNTAX_ERROR);
		}
	}
}
/******************************************************************************
*
* dsNCopyName - copy C name
*
*/
int dsNCopyName(char *dst, char *str, char **ptr, int maxLen)
{
	int n;

	if (ptr) {
		*ptr = str;
	}
	for (;isspace(*str); str++);
	for (n = 0; isalnum(str[n]) || str[n] == '_'; n++);

	if (n == 0 || n > DS_MAX_NAME_SIZE || isdigit(*str)) {
		DS_ERROR(DS_E_INVALID_NAME);
	}
	if (n >= maxLen) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	strncpy (dst, str, n);
	dst[n] = '\0';
	if (ptr) {
		*ptr = str + n;
	}
	return TRUE;
}
/******************************************************************************
*
* dsNextChar - check str for character
*
*/
int dsNextChar(char *str, char **ptr, int c)
{
	if (ptr) {
		*ptr = str;
	}
	for(; isspace(*str); str++);
	if (*str == c) {
		if (ptr) {
			*ptr = str + 1;
		}
		return TRUE;
	}
	return FALSE;
}
/******************************************************************************
*
* dsNextName - try to match name with str
*
*/
int dsNextName(char *str, char **ptr, char *name)
{
	if (ptr) {
		*ptr = str;
	}
	for (; isspace(*str); str++);
	while (*name != '\0') {
		if (*str++ != *name++)
			return FALSE;
	}
	if (isalnum(*str) || *str == '_') {
		return FALSE;
	}
	if (ptr) {
		*ptr = str;
	}
	return TRUE;
}
/******************************************************************************
*
* dsNumToMem - format num and append it to str in mem
*
*/
int dsNumToMem(DS_MEM_T *mem, size_t num)
{
	char buf[20];

	sprintf(buf, "%d", num);
	return dsStrToMem(mem, buf);
}
/******************************************************************************
*
* dsStrSearch - find string in an array of structs
*
*/
int dsStrSearch(size_t *pIndex, char *str, char *base, size_t stride, size_t count)
{
	size_t i;
	
	for (i = 0; i < count; i++, base += stride) {
		if (strcmp(base, str) == 0) {
			*pIndex = i;
			return TRUE;
		}
	}
	return FALSE;
}
/******************************************************************************
*
* dsStrToMem - append string to mem
*
*/
int dsStrToMem(DS_MEM_T *bp, char *str)
{
	int n;

	n = strlen(str);
	if (bp->next + n >= bp->limit) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	strcpy(bp->next, str);
	bp->next += n;
	return TRUE;
}
/******************************************************************************
*
* dsTypeCalloc - allocate zeroed memory for type structure
*
*/
char *dsTypeCalloc(size_t size)
{
	char *ptr;

	if ((ptr = calloc(1, size)) != NULL) {
		dsTidSize += size;
	}
	else {
		DS_LOG_ERROR(DS_E_MALLOC_FAILURE);
	}
	return ptr;
}
/******************************************************************************
*
* dsTypeFree - free memory for type structure
*
*/
void dsTidFree(void *ptr, size_t size)
{
	if (ptr != NULL) {
		free((char *)ptr);
		dsTidSize -= size;
	}
}
