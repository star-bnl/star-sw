/* Copyright 1993, Lawrence Berkeley Laboratory */

/* c.dstid - routines to define data type IDs */

/*
modification history
--------------------
24apr93,whg  written.
28feb95,whg  change to CORBA IDL
11jun96,whg  added indirection to dataset structure
18jun98,whg  modify to allow C++ wrappers
*/
/*
DESCRIPTION
routines to manage type ID structures and hash tables
*/
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#include "dstype.h"
/******************************************************************************
*
* hash table for declaration cache
*
*/
/* replace by real semaphore for multi thread */
#define DS_TID_GIVE (++semGive == semTake)
#define DS_TID_TAKE (semTake++ == semGive)
static int semGive = 0, semTake = 0;

typedef struct tid_hash {
	struct tid_hash *next;	/* next node */
	size_t tid;				/* type ID for this entry */
	size_t len;				/* length of str */
	char *spec;				/* type specifier */
}TID_HASH_T;

static size_t dsTidHit = 0, dsTidMax = 0, dsTidMiss = 0;
static TID_HASH_T **dsTidHash = NULL;
/******************************************************************************
*
* dsFormatStruct - recursive part of format type declaration string
*
* RETURNS: TRUE if success else FALSE
*/
static int dsFormatStruct(DS_TYPE_T *type, DS_TYPE_T **scope, size_t nScope,
						  size_t level, char **in, char *limit)
{
	int j, simple;
	size_t firstDecl, i;
	DS_TYPE_T *ft;
	DS_FIELD_T *field = type->field;

	if (!dsPutStr("struct ", in, limit) ||
		!dsPutStr(type->name, in, limit) ||
		!dsPutStr(" {\n", in, limit)) {
		DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
	}
	for (i = 0; i < type->nField;) {
		ft = field[i].type;
		if (!dsPutTabs(level + 1, in, limit)) {
			DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
		}
		if (ft->code != DS_TYPE_STRUCT) {
			simple = TRUE;
		}
		else {
			for (j = nScope; j-- > 0 && ft != scope[j];);
			simple = j >= 0;
		}
		if (simple) {
			if (!dsPutStr(ft->name, in, limit)) {
				DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
			}
		}
		else {
			if (!dsFormatStruct(ft, scope,
				nScope, level + 1, in, limit)) {
				return FALSE;
			}
			if (nScope >= DS_SCOPE_DIM) {
				DS_LOG_ERROR(DS_E_SCOPE_TOO_LARGE);
			}
			scope[nScope++] = ft;
		}
		for (firstDecl = i; i < type->nField && ft == field[i].type; i++) {
			if (!dsPutStr(i == firstDecl ? " " : ", ", in, limit) ||
				!dsPutStr(field[i].name, in, limit)) {
				DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
			}
			for (j = 0; j < DS_MAX_DIMS && field[i].dim[j]; j++) {
				if (!dsPutStr("[", in, limit) ||
					!dsPutNumber(field[i].dim[j], in, limit) ||
					!dsPutStr("]", in, limit)) {
					DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
				}
			}
		}
		if (!dsPutStr(";\n", in, limit)) {
			DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
		}
	}
	if (!dsPutTabs(level, in, limit) ||
		!dsPutStr("}", in, limit)) {
		DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
	}
	return TRUE;
}
/******************************************************************************
*
* dsFormatTypeSpecifier - format a declaration string for a structure
*
* RETURNS: TRUE if success else FALSE
*/
int dsFormatTypeSpecifier(char *str, size_t maxSize, DS_TYPE_T *type)
{
	char *in = str, *limit = str + maxSize;
	size_t tid;
	DS_TYPE_T *scope[DS_SCOPE_DIM];

	if (type->code != DS_TYPE_STRUCT) {
		DS_ERROR(DS_E_INVALID_TYPE);
	}
	if (!dsFormatStruct(type, scope, 0, 0, &in, limit)) {
		return FALSE;
	}
	if (!dsTypeId(&tid, str, NULL)) {
		DS_ERROR(DS_E_INVALID_TYPE);
	}
	if (type->tid != 0 && tid != type->tid) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	return TRUE;
}
/******************************************************************************
*
* dsHashSpec - hash function for type specifier
*
* RETURNS: TRUE if success else FALSE
*/
static int dsHashSpec(size_t *pHash, char *spec)
{
	char name[DS_NAME_DIM], *str;
	size_t size;

	if (dsTidHash == NULL) {
		if (!DS_TID_TAKE) {
			DS_ERROR(DS_E_SEM_TAKE_ERROR);
		}
		size = DS_TID_HASH_DIM*sizeof(TID_HASH_T *);
		dsTidHash = dsTypeCalloc(size);
		if (!DS_TID_GIVE) {
			DS_ERROR(DS_E_SEM_GIVE_ERROR);
		}
		if (dsTidHash == NULL) {
			return FALSE;
		}
	}
	if (!dsParseName(name, spec, &str) ||
		strcmp(name, "struct") != 0 ||
		!dsParseName(name, str, &str)) {
		DS_ERROR(DS_E_INVALID_TYPE_SPECIFIER);
	}
	*pHash = dsHash(name)%DS_TID_HASH_DIM;
	return TRUE;
}
/******************************************************************************
*
* dsHashTid - add (tid, spec) to hash
*
* RETURNS: TRUE if success else FALSE
*/
static int dsHashTid(TID_HASH_T **ppNode, size_t tid, char *spec, size_t len)
{
	size_t h;
	TID_HASH_T **next, *node;

	if(!dsHashSpec(&h, spec)) {
		return FALSE;
	}
	if (!DS_TID_TAKE) {
		DS_ERROR(DS_E_SEM_TAKE_ERROR);
	}
	next = &dsTidHash[h];
	for (next = &dsTidHash[h]; (node = *next) != NULL; next = &node->next ) {
		if (tid == node->tid && strncmp(spec, node->spec, node->len) == 0) {
			goto done;
		}
	}
	if (spec[len-1] != '}') {
		DS_LOG_ERROR(DS_E_SYSTEM_ERROR);
		goto fail;
	}
	if ((node = dsTypeCalloc(sizeof(TID_HASH_T) + len +1)) == NULL) {
		goto fail;
	}
	if (tid > dsTidMax)dsTidMax = tid;
	node->spec = (char *)&node[1];
	strncpy(node->spec, spec, len);
	node->spec[len] = '\0';
	node->tid = tid;
	node->len = len;
	*next = node;/* must be after node is setup to be thread safe */

done:
	if (!DS_TID_GIVE) {
		DS_ERROR(DS_E_SEM_GIVE_ERROR);
	}
	*ppNode = node;
	return TRUE;
fail:
	if (!DS_TID_GIVE) {
		DS_ERROR(DS_E_SEM_GIVE_ERROR);
	}
	return FALSE;
}
/******************************************************************************
*
* dsTypeSpecifier - get pointer and length of specifier string for a tid
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeSpecifier(char **ptr, size_t tid)
{
	char buf[DS_MAX_SPEC_LEN+1];
	TID_HASH_T *node;
	DS_TYPE_T *type;

	if (!dsTypePtr(&type, tid)) {
		return FALSE;
	}
	if (type->stdspec == NULL) {
		if (!dsFormatTypeSpecifier(buf, sizeof(buf), type) ||
			!dsHashTid(&node, tid, buf, strlen(buf))) {
			return FALSE;
		}
		type->stdspec = node->spec;
	}
	if (ptr != NULL) {
		*ptr = type->stdspec;
	}
	return TRUE;
}
/******************************************************************************
*
* dsTidHashStats - print performance stats for tid hash
*
* RETURNS: TRUE
*/
int dsTidHashStats()
{
	printf("tidHashStats: tidMax %d, tidMiss %d, tidHit %d\n",
		dsTidMax, dsTidMiss, dsTidHit);
	return TRUE;
}
/******************************************************************************
*
* dsTypeId - get tid for declaration string
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeId(size_t *pTid, char *str, char **ptr)
{
	char *tmp;
	size_t h;
	TID_HASH_T **next, *node;
	DS_TYPE_T *type;

	for (;isspace(*str); str++);
	if (!dsHashSpec(&h, str)) {
		return FALSE;
	}
	for (next = &dsTidHash[h]; (node = *next) != NULL; next = &node->next ) {
		if (strncmp(str, node->spec, node->len) == 0) {
			dsTidHit++;
			break;
		}
	}
	if (node == NULL) {
		if (!dsParseType(&type, str, &tmp) ||
			!dsHashTid(&node, type->tid, str, tmp - str)) {
			return FALSE;
		}
		dsTidMiss++;
	}
	if (pTid != NULL) {
		*pTid = node->tid;
	}
	if (ptr != NULL) {
		*ptr = str + node->len;
	}
	return TRUE;
}
/******************************************************************************
*
* dsTypeListCreate - initialize type listhash table (N^2 best for listDim)
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeListCreate(size_t **pList, size_t listDim)
{
	size_t *list = *pList, n, size;

	if (listDim < 4) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (list == NULL) {
		if ((listDim & (listDim - 1)) != 0) {
			/* make dim next power of two */
			for (n = 2; 2*n != 0 && listDim <= n; n += n);
			listDim = n;
		}
		size = listDim*sizeof(list[0]);
		if ((list = dsTypeCalloc(size)) == NULL) {
			return FALSE;
		}
		*pList = list;
	}
	else {
		size = listDim*sizeof(list[0]);
		memset((char *)list, 0, size);
	}
	list[0] = listDim - 1;
	return TRUE;
}
/******************************************************************************
*
* dsTypeListEnter - parse type declaration and enter tid in type list
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeListEnter(size_t *list, char *str, char **ptr)
{
	size_t h, tid;
	DS_TYPE_T *type;

	if (!dsTypeId(&tid, str, ptr)) {
		return FALSE;
	}
	if (!dsTypePtr(&type, tid)) {
		return FALSE;
	}
	if (!dsTypeListFind(&h, list, type->name)) {
		return FALSE;
	}
	if (list[h] == 0) {
		list[h] = tid;
	}
	else if (list[h] != tid) {
		DS_ERROR(DS_E_DUPLICATE_TYPE_NAME);
	}
	return TRUE;
}
/******************************************************************************
*
* dsTypeListFind - find entry with type name in str
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeListFind(size_t *pH, size_t *list, char *str)
{
	int c;
	size_t h, len, tid, n = list[0];
	DS_TYPE_T *type;

	for (; isspace(*str); str++);
	for (h = len = 0; isalnum(c = str[len]) || c == '_'; len++) {
		h = ((h << 8) + c)%n;
	}
	for (h++; (tid = list[h]) != 0; h = h > 1 ? h - 1 : list[0]) {
		if (!dsTypePtr(&type, tid)) {
			return FALSE;
		}
		if ((c = dsCmpName(str, type->name)) <= 0) {
			if (c < 0) {
				DS_ERROR(DS_E_NAMES_COLLIDE);
			}
			break;
		}
		if (n-- == 0) {
			DS_ERROR(DS_E_TOO_MANY_TYPES);
		}
	}
	*pH = h;
	return TRUE;
}
/******************************************************************************
*
* dsTypeListFree - free type list
*
* RETURNS: TRUE
*/
int  dsTypeListFree(size_t *list)
{
	dsTypeFree(list, (1 + list[0])*sizeof(list[0]));
	return TRUE;
}
