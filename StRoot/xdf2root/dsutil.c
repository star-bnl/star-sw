/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsutil.c - data structure utility routines */
   
/*
modification history
--------------------
24apr93,whg  written.
15feb95,whg	 added functions for CORBA IDL
11jun96,whg  added indirection to dataset structure
*/
/*
DESCRIPTION
general utility routines
*/
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#include "dstype.h"
/*****************************************************************************
*
* dsCmpName - CORBA style compare of names
*
* RETURNS: 1 if not equal, zero if equal and -1 if names collide
*/	
int dsCmpName(const char *s1, const char *s2)
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
int dsCopyName(char *dst, const char *str, const char **ptr)
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
* dsGetColumnSpecifier - return col specifier (colName or tableName.tableName)
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
int dsGetNumber(size_t *pNumber, DS_BUF_T *bp)
{
	int c;
	size_t number = 0;

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
/*****************************************************************************
*
* dsMark - mark node as visited
*
* RETURN TRUE if success else FALSE
*/
int dsMark(DS_LIST_T *list, DS_DATASET_T *node)
{
	if (!DS_IS_VALID(node)) {
		DS_ERROR(DS_E_INVALID_DATASET_OR_TABLE);
	}
	if ((node->visit - 1) < list->count &&
		list->pItem[node->visit - 1] == node) {
		DS_ERROR(DS_E_MARK_ERROR);
	}
	if (!dsListAppend(list, node)) {
		return FALSE;
	}
	node->visit = list->count;
	return TRUE;
}
/*****************************************************************************
*
* dsPutc - put a character in the buffer defined by bp
*
* RETURNS: the character for success or EOF if an error occured
*/
int dsPutc(int c, DS_BUF_T *bp)
{
	size_t size;

	if (bp->in >= bp->limit) {
		size = bp->limit - bp->first + DS_BUF_ALLOC;
		if (!dsBufRealloc(bp, size)) {
			return EOF;
		}
	}
	return  0XFF & (*bp->in++ = (char)c);
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
/******************************************************************************
*
* dsVisitClear - clear visit during depth first traversal of dataset
*
* RETURN TRUE if success else FALSE
*/
int dsVisitClear(DS_DATASET_T *dataset)
{
	size_t i;

	if (!DS_IS_VALID(dataset)) {
		DS_ERROR(DS_E_INVALID_DATASET_OR_TABLE);
	}
	if (dataset->visit != 0) {
		dataset->visit = 0;
		if (DS_IS_DATASET(dataset)) {
			for (i = 0; i < dataset->elcount; i++) {
				if (!dsVisitClear(dataset->p.link[i])) {
					return FALSE;
				}
			}
		}
	}
	return TRUE;
}
/******************************************************************************
*
* dsVisitCount - count visits during depth first traversal of dataset
*
* RETURN TRUE if success else FALSE
*/
int dsVisitCount(DS_DATASET_T *dataset)
{
	size_t i;

	if (!DS_IS_VALID(dataset)) {
		DS_ERROR(DS_E_INVALID_DATASET_OR_TABLE);
	}
	if (dataset->visit++ == 0) {
		if (DS_IS_DATASET(dataset)) {
			for (i = 0; i < dataset->elcount; i++) {
				if (!dsVisitCount(dataset->p.link[i])) {
					return FALSE;
				}
			}
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsVisited - check for node of dataset visited
*
* RETURN TRUE if visited else FALSE
*/
int dsVisited(DS_LIST_T *list, DS_DATASET_T *node)
{
	return node != NULL && (node->visit - 1) < list->count &&
		node == list->pItem[node->visit - 1];
}
/******************************************************************************
*
* dsVisitList - form a list of nodes accessable from dataset
*
* RETURN TRUE if success else FALSE
*/
int dsVisitList(DS_LIST_T *list, DS_DATASET_T *dataset)
{
	size_t i;

	if (!dsMark(list, dataset)) {
		return FALSE;
	}
	if (DS_IS_DATASET(dataset)) {
		for (i = 0; i < dataset->elcount; i++) {
			if (!dsVisited(list, dataset->p.link[i]) &&
				!dsVisitList(list, dataset->p.link[i])) {
				return FALSE;
			}
		}
	}
	return TRUE;
}
