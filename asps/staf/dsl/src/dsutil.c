/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsutil.c - data structure utility routines */
   
/*
modification history
--------------------
24apr93,whg  written.
15feb95,whg	 added functions for CORBA IDL
11jun98,whg  restructured
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
/*****************************************************************************
*
* dsHash - hash function for strings
*
* RETURNS: hash of string
*/	
size_t dsHash(char *str)
{
	int c;
	size_t h;

	/* djb2 hash algorithm by Dan Bernstein */
	for (h = 5381; c = *str; str++) {
		h += (h << 5) + c;
	}
	return h;
}
/*****************************************************************************
*
* dsIsNextName - check for next name
*
* RETURNS: TRUE if name is next else FALSE
*/
int dsIsNextName(char *name, char *str, char **ptr)
{
	for (;isspace(*str); str++);
	for (;*str == *name && *str != '\0'; name++, str++);
	if (*name != '\0' || isalpha(*str) || *str == '_') {
		return FALSE;
	}
	if (ptr != NULL) {
		*ptr = str;
	}
	return TRUE;
}
/*****************************************************************************
*
* dsNonSpace - return next non-space character
*
* RETURNS: non-space character for success or EOF
*/
int dsNonSpace(char *str, char **ptr)
{
	while (isspace(*str++));
	if (ptr != NULL) {
		*ptr = str;
	}
	return *(str - 1);
}
/*****************************************************************************
*
* dsParseName - get a CORBA style name
*
* RETURNS: length of the name for success or EOF
*/
int dsParseName(char *name, char *str, char **ptr)
{
	int c, i = 0;
	
	if (!isalpha(c = dsNonSpace(str, &str))) {
		DS_ERROR(DS_E_INVALID_NAME);
	}
	do {
		if (i >= DS_MAX_NAME_LEN) {
			DS_ERROR(DS_E_INVALID_NAME);
		}
		name[i++] = (char)c;
		c = *str++;
	} while (isalnum(c) || c == '_');
	name[i] = '\0';
	if (ptr != NULL) {
		*ptr = str - 1;
	}
	return TRUE;
}
/*****************************************************************************
*
* dsParseNumber - get an unsigned number
*
* RETURNS: TRUE if success else FALSE
*/
int dsParseNumber(size_t *pNumber, char *str, char **ptr)
{
	int c;
	size_t number = 0;

	if (!isdigit(c = dsNonSpace(str, &str))) {
		DS_ERROR(DS_E_INVALID_NUMBER)
	}
	do {
		if (number > (UINT_MAX/10 -1)) {
			DS_ERROR(DS_E_INVALID_NUMBER)
		}
		number = 10*number + (c - '0');
	} while(isdigit(c = *str++));
	*pNumber = number;
	if (ptr != NULL) {
		*ptr = str - 1;
	}
	return TRUE;
}
/*****************************************************************************
*
* dsPutc - put a character in string
*
* RETURNS: TRUE for success else FALSE
*/
int dsPutc(int c, char **in, char *limit)
{
	char *ptr = *in;

	if ((ptr + 1) >= limit) {
		return FALSE;
	}
	*ptr++ = (char)c;
	*ptr = '\0';
	*in = ptr;
	return  TRUE;
}
/*****************************************************************************
*
* dsPutNumber - put a number in string
*
* RETURNS: TRUE for success else FALSE
*/
int dsPutNumber(int n, char **in, char *limit)
{
	char buf[20];
	
	sprintf(buf, "%d", n);
	return dsPutStr(buf, in, limit);
}
/*****************************************************************************
*
*  dsPutStr - append a string
*
* RETURNS: TRUE for success else FALSE
*/
int dsPutStr(char *str, char **in, char *limit)
{
	char *ptr = *in;

	while(*str != '\0' && ptr < limit) {
		*ptr++ = *str++;
	}
	if (ptr >= limit) {
		return FALSE;
	}
	*ptr = '\0';
	*in = ptr;
	return TRUE;
}
/*****************************************************************************
*
* dsPutTabs - put n tabs in string
*
* RETURNS: TRUE for success else FALSE
*/ 
int dsPutTabs(size_t n, char **in, char *limit)
{
	char *ptr = *in;

	while (n-- > 0 && ptr < limit) {
		*ptr++ = '\t';
	}
	if (ptr >= limit) {
		return FALSE;
	}
	*ptr = '\0';
	*in = ptr;
	return TRUE;
}
