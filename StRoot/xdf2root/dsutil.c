/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsutil.c - data structure utility routines */
   
/*
modification history
--------------------
24apr93,whg  written.
15feb95,whg	 added functions for CORBA IDL
*/

/*
DESCRIPTION
general routines for parsing and memory allocation
*/
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#include "dscodes.h"
#include "dstype.h"

static int dsAllocCalls = 0;
static int dsFreeCalls = 0;
static int dsTidSize = 0;
/******************************************************************************
*
* dsCheckDuplicate - verify dataset or type names are unique 
*
* RETURNS: TRUE if unique names, FALSE if duplicate or names collide
*/
int dsCheckDuplicate(char *name, size_t count, size_t stride)
{
	char *n1, *n2;
	int c;
	size_t i;
	
	for (i = 1, n2 = name; i < count; i++) {
		n2 += stride;
		for (n1 = name; n1 < n2; n1 += stride) {
			if ((c = dsCmpName(n1, n2)) <= 0) {
				if (c < 0) {
					DS_ERROR(DS_E_NAMES_COLLIDE);
				}
				else {
					DS_ERROR(DS_E_DUPLICATE_NAME);
				}
			}
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsCmpName - CORBA style compare of names
*
* RETURNS: 1 if not equal, zero if equal and -1 if names collide
*/	
int dsCmpName(char *s1, char *s2)
{
	int rtn;
	
	for (rtn = 0; ;s1++, s2++) {
		if (*s1 == *s2) {
			if (*s1 == '\0') {
				return rtn;
			}
		}
		else if (tolower(*s1) != tolower(*s2)) {
			return 1;
		}
		else {
			rtn = -1;
		}
	}
}
/******************************************************************************
*
* dsCopyName - get standard C identifier
*
* RETURNS: TRUE if copy successuful, FALSE if invalid name or name too long
*/
int dsCopyName(char *dst, char *str, char **ptr)
{
	DS_BUF_T bp;
	int c;
    
    DS_GET_INIT(&bp, str);
	if ((c = dsGetName(dst, &bp)) >= 0) {
		str = bp.out;
	}
	if (ptr) {
		*ptr = str;
	}
	return c < 0 ? FALSE : TRUE;
}
/******************************************************************************
*
* dsDatasetAllocStats - print dataset allocation stats
*
* RETURNS: N/A
*/
void dsDatasetAllocStats(void)
{
	printf("AllocStats:tidSize: %d, allocCalls %d, freeCalls %d, diff %d\n",
		dsTidSize, dsAllocCalls, dsFreeCalls, dsAllocCalls - dsFreeCalls);
}
/******************************************************************************
*
* dsDsetAlloc - allocate memory for dataset or table
*
* RETURNS: pointer to memory or NULL if size is zero or not enough memory
*/
char *dsDsetAlloc(size_t size)
{
	return dsDsetRealloc(NULL, size); 
}
/******************************************************************************
*
* dsDsetFree - free memory for dataset or table
*
* RETURNS: N/A
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
* dsDsetRealloc - allocate memory for dataset or table
*
* RETURNS: pointer to memory or NULL if size is zero or not enough memory
*/
char *dsDsetRealloc(char *old, size_t size)
{
	char *ptr;
	
	if (size == 0){
		DS_LOG_ERROR(DS_E_ZERO_LENGTH_ALLOC);
	}
	if ((ptr = (old == NULL ? malloc(size) : realloc(old, size))) != NULL) {
		if (old == NULL) {
			dsAllocCalls++;
		}
		return ptr;
	}
	else {
		DS_LOG_ERROR(DS_E_NOT_ENOUGH_MEMORY);
	}
	return NULL;	
}
/*****************************************************************************
*
* dsGetChar - get a character from the buffer defined by bp
*
* RETURNS: character >= 0 for success or EOF if an error occured
*/
int dsGetc(DS_BUF_T *bp)
{
	return DS_GETC(bp);
}
/*****************************************************************************
*
* dsGetColumnSpecifier
*
* RETURNS:
*/ 
int dsGetColumnSpecifier(char *tableName, char *columnName, DS_BUF_T *bp)
{	
	if (dsGetName(columnName, bp) <= 0) {
		DS_ERROR(DS_E_INVALID_COLUMN_SPECIFIER);
	}
	if (DS_PEEK(bp) != '.') {
		tableName[0] = '\0';
		return TRUE;
	}
	DS_GETC(bp);
	strcpy(tableName, columnName);
	if (!isalpha(DS_PEEK(bp)) || dsGetName(columnName, bp) <= 0) {
		DS_ERROR(DS_E_INVALID_COLUMN_SPECIFIER);
	}
    return TRUE;
}
/*****************************************************************************
*
* dsGetName - get a CORBA style name
*
* RETURNS: length of the name for success or EOF
*/
int dsGetName(char *name, DS_BUF_T *bp)
{
	int c, i = 0;
	
	if (!isalpha(c = dsGetNonSpace(bp))) {
		dsUngetc(c, bp);
		return EOF;
	}
	do {
		if (i < DS_MAX_NAME_LEN) {
			name[i++] = (char)c;
		}
		c = DS_GETC(bp);
	} while (isalnum(c) || c == '_');
	dsUngetc(c, bp);
	if (i <= DS_MAX_NAME_LEN) {
		name[i] = '\0';
		return i;
	}
	return EOF;
}
/*****************************************************************************
*
* dsGetNonSpace - return next non-space character
*
* RETURNS: non-space character for success or EOF
*/
int dsGetNonSpace(DS_BUF_T *bp)
{
	int c;
	
	while (isspace(c = DS_GETC(bp)));
	return c;
}
/*****************************************************************************
*
* dsGetNumber - get an unsigned number
*
* RETURNS: zero for success or EOF 
*/
int dsGetNumber(unsigned *pNumber, DS_BUF_T *bp)
{
	int c;
	unsigned number = 0;

	if (!isdigit(c = dsGetNonSpace(bp))) {
		dsUngetc(c, bp);
		return EOF;
	}
	do {
		if (number < (UINT_MAX/10)) {
			number = 10*number + (c - '0');
		}
		else {
			/* overflow - return max unsigned */
			number = UINT_MAX;
		} 
	} while(isdigit(c = DS_GETC(bp)));
	dsUngetc(c, bp);
	*pNumber = number;
	return 0;
}
/******************************************************************************
*
* dsFirstPass - first pass for parse of dataset and type specifiers
*
* RETURNS: TRUE for success or FALSE for error
*/
int dsFirstPass(int *pSepCount, int *map,
	int *pMapCount, int maxMapCount, char *str)
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
					DS_ERROR(DS_E_TOO_MANY_MEMBERS);
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
/*****************************************************************************
*
* dsPutc - put a character in the buffer defined by bp
*
* RETURNS: the character for success or EOF if an error occured
*/
int dsPutc(int c, DS_BUF_T *bp)
{
	return DS_PUTC(c, bp);
}
/*****************************************************************************
*
* dsPutNumber - put a number in the buffer defined by bp
*
* RETURNS: nonnegative for success else EOF
*/
int dsPutNumber(int n, DS_BUF_T *bp)
{
	char buf[20];
	
	sprintf(buf, "%d", n);
	return dsPuts(buf, bp);
}
/*****************************************************************************
*
*  dsPuts - put a string in the buffer defined by bp
*
* RETURNS: nonnegative for success else EOF
*/
int dsPuts(char *s, DS_BUF_T *bp)
{
	int c;
	
	while ((c = *s++) != '\0') {
		if (DS_PUTC(c, bp) < 0) {
			return EOF;
		}
	}
	return 0;
}
/*****************************************************************************
*
* dsPutTabs - put n tabs in the buffer defined by bp
*
* RETURNS: nonnegative for success else EOF
*/ 
int dsPutTabs(int n, DS_BUF_T *bp)
{
	while (n-- > 0) {
		if (DS_PUTC('\t', bp) < 0) {
			return EOF;
		}
	}
	return 0;
}
/******************************************************************************
*
* dsTypeCalloc - allocate zeroed memory for type structure
*
* RETURNS: pointer to memory or NULL if size is zero or not enough memory
*/
char *dsTypeCalloc(size_t size)
{
	char *ptr;

	if (size == 0){
		DS_LOG_ERROR(DS_E_ZERO_LENGTH_ALLOC);
	}
	else if ((ptr = calloc(1, size)) != NULL) {
		dsTidSize += size;
		return ptr;
	}	
	else {
		DS_LOG_ERROR(DS_E_NOT_ENOUGH_MEMORY);
	}
	return NULL;
}
/******************************************************************************
*
* dsTypeFree - free memory for type structure
*
* RETURNS: N/A
*/
void dsTypeFree(void *ptr, size_t size)
{
	if (ptr != NULL) {
		free((char *)ptr);
		dsTidSize -= size;
	}
}
/******************************************************************************
*
* dsUngetc - push character back
*
* RETURNS: character if success else EOF
*/
int dsUngetc(int c, DS_BUF_T *bp)
{
	if (c != EOF && bp->out > bp->first && (0XFF & c) == *(bp->out - 1)) {
		bp->out--;
		return c;
	}
	return EOF;
} 
