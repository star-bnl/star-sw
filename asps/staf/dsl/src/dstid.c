/* Copyright 1993, Lawrence Berkeley Laboratory */

/* c.dstid - routines to define data type IDs */

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
#include "dscodes.h"
#include "dstype.h"

static int dsTidInit(void);

/******************************************************************************
*
* Tid list and hash table for declaration cache
*
*/
typedef struct tid_hash {
	struct tid_hash *next;	/* next node - MUST BE FIRST MEMBER OF STRUCT */
	size_t tid;		/* type ID for this entry */
	size_t len;		/* length of str */
	char str[1];	/* declaration string for this entry */
}TID_HASH_T;

typedef struct tid_info {
	DS_TYPE_T *type;	/* pointer to type structure */
	char *def;		/* string for tid type definition */
	int len;		/* length of def string */
}TID_INFO_T;

static size_t dsTidCount = 0;	/* index of next dsTidInfo element */
static int dsTidHit = 0;
static int dsTidMiss = 0;
static TID_HASH_T **dsTidHash = NULL;
static TID_INFO_T *dsTidInfo = NULL;
/******************************************************************************
*
* dsTidInit - intilize dsTidHash and dsTidInfo structs
*
*/
static int dsTidInit()
{
	size_t hashSize = DS_TID_HASH_LEN*sizeof(TID_HASH_T *);
	size_t infoSize = (DS_MAX_TID + 1)*sizeof(TID_INFO_T);

	/********* start critical section *********/
	if (!dsTypeLock()) {
		return FALSE;
	}
	if (dsTidCount != 0) {
		/* exit if initialized */
		return dsTypeUnlock();
	}
	if ((dsTidHash = (TID_HASH_T **)dsTypeCalloc(hashSize)) != NULL) {
		if ((dsTidInfo =(TID_INFO_T *)dsTypeCalloc(infoSize)) == NULL) {
			dsTidFree(dsTidHash, hashSize);
			dsTidHash = NULL;
		}
	}
	if (dsTidHash == NULL) {
		dsTypeUnlock();
		return FALSE;
	}
	/* set dsTidCount to indicate initilized */
	dsTidCount = 1;
	return dsTypeUnlock();
	/********** end critical section **********/
}
/******************************************************************************
*
* dsTypeDef - return pointer to declaration string for a tid
*
*/
int dsTypeDef(char **ptr, size_t *pLen, size_t tid)
{
	char buf[DS_MAX_DECL_LEN+1], *str;
	size_t len;

	if (tid < 1 || tid >= dsTidCount) {
		DS_ERROR(DS_E_INVALID_TYPE_ID);
	}
	/* check if string is already formated */
	if (dsTidInfo[tid].def == NULL) {
		if (!dsFmtTypeDef(buf, sizeof(buf), dsTidInfo[tid].type)) {
			return FALSE;
		}
		len = strlen(buf);
		if ((str = dsTypeCalloc(len + 1)) == NULL) {
			return FALSE;
		}
		strcpy(str, buf);

		/******** start critical section *********/
		if (!dsTypeLock()) {
			goto fail;
		}
		/* check for string formatted by another thread */
		if (dsTidInfo[tid].def == NULL) {
			dsTidInfo[tid].def = str;
			dsTidInfo[tid].len = len;
			str = NULL;
		}
		if (!dsTypeUnlock()) {
			goto fail;
		}
		/********* end critical section **********/
		if (str != NULL) {
			/* free memory - def was created by another thread */
			dsTidFree(str, len + 1);
		}
	}
	if (ptr != NULL) {
		*ptr = dsTidInfo[tid].def;
	}
	if (pLen != NULL) {
		*pLen = dsTidInfo[tid].len;
	}
	return TRUE;

fail:
	if (str != NULL) {
		dsTidFree(str, len + 1);
	}
	return FALSE;
}
/******************************************************************************
*
* dsTypePtr - return a pointer to a type structure for a tid
*
*/
int dsTypePtr(DS_TYPE_T **pType, size_t tid)
{
	if (tid < 1 || tid >= dsTidCount) {
		DS_ERROR(DS_E_INVALID_TYPE_ID);
	}

	*pType = dsTidInfo[tid].type;
	return TRUE;
}
/******************************************************************************
*
* dsTidHashStats - print performance stats for tid hash
*
*/
int dsTidHashStats()
{
	printf("tidHashStats: tidHit %d, tidMiss %d\n", dsTidHit, dsTidMiss);
	return TRUE;
}
/******************************************************************************
*
* dsTypeId - return tid for declaration string
*
*/
int dsTypeId(size_t *pTid, char *str, char **ptr)
{
	char *next;
	size_t c, i, h, n, newSize, typeSize;
	TID_HASH_T *entry, *sptr, *new, *tptr;
	DS_TYPE_T *type;

	if (ptr != NULL) {
		*ptr = str;
	}
	/* skip to type name */
	for (; isspace(*str); str++);
	for (i = 0; isalnum(c = str[i]) || c == '_'; i++);
	for (; isspace(str[i]); i++);

	/* hash type name */
	for (h = 0, n = i; isalnum(c = str[i]) ||c == '_'; i++) {
		h = ((h << 8) + c)%DS_TID_HASH_LEN;
	}
	if (i == n) {
		DS_ERROR(DS_E_INVALID_TYPE_NAME);
	}
	if (dsTidCount == 0) {
		if (!dsTidInit()) {
			return FALSE;
		}
	}
	/* search for string in hash table */
	for (sptr = (TID_HASH_T *)&dsTidHash[h]; sptr->next != NULL; ) {
		sptr = sptr->next;
		if (strncmp(str, sptr->str, sptr->len) == 0) {
			if (pTid != NULL) {
				*pTid = sptr->tid;
			}
			if (ptr != NULL) {
				*ptr = str + sptr->len;
			}
dsTidHit++;
			return TRUE;
		}
	}
dsTidMiss++;

	type = NULL;
	if (!dsCreateType(&type, &typeSize, str, &next)) {
		return FALSE;
	}
	n = next - str;

	/* create entry for open hash */
	newSize = sizeof(TID_HASH_T) + n;
	if ((new = (TID_HASH_T *)dsTypeCalloc(newSize)) == NULL) {
		dsTidFree(type, typeSize);
		return FALSE;
	}
	strncpy(new->str, str, n);
	new->len = n;
	tptr = (TID_HASH_T *)&dsTidHash[h];

	for(entry = NULL;;) {
		/* look for type match in hash table */
		while (new->tid == 0 && tptr->next != NULL) {
			tptr = tptr->next;
			if (dsTypeCmp(type, dsTidInfo[tptr->tid].type) == 0) {
				new->tid = tptr->tid;
				dsTidFree(type, typeSize);
				type = NULL;
			}
		}
		/********* start critical section *********/
		if( !dsTypeLock()) {
			goto fail;
		}
		/* check if entries were created by other threads */
		if (sptr->next == NULL) {
			/* create a new tid if type not found in hash table */
			if (new->tid == 0) {
				if (dsTidCount >= DS_MAX_TID) {
					/* exit with error - dsTidInfo full */
					if (!dsTypeUnlock()) {
						goto fail;
					}
					DS_LOG_ERROR(DS_E_TOO_MANY_TYPES);
					goto fail;
				}
				new->tid = dsTidCount;
				dsTidInfo[dsTidCount].type = type;
				/* allow other threads to use new tid */
				dsTidCount++;
			}
			/* add entry to open hash */
			sptr->next = new;
			if (!dsTypeUnlock())  {
				goto fail;
			}
			entry = new;
			break;
		}
		if (!dsTypeUnlock()) {
			goto fail;
		} 
		/********** end critical section **********/

		/* check entry created by other thread */
		sptr = sptr->next;
		if (strncmp(str, sptr->str, sptr->len) == 0) {
			dsTidFree(new, newSize);
			if (type) {
				dsTidFree(type, typeSize);
			}
			entry = sptr;
			break;
		}
	}
	if (ptr != NULL) {
		*ptr = next;
	}
	if (pTid != NULL) {
		*pTid = entry->tid;
	}
	return TRUE;

fail:
	if (type != NULL) {
		dsTidFree(type, typeSize);
	}
	if (new != NULL) {
		dsTidFree(new, newSize);
	}
	return FALSE;
}
/******************************************************************************
*
* dsTypeListCreate - initialize type listhash table (N^2 best for listDim)
*
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
		if ((list = (size_t *)dsDsetAlloc(size)) == NULL) {
			return FALSE;
		}
		*pList = list;
	}
	else {
		size = listDim*sizeof(list[0]);
	}
	memset((char *)list, 0, size);
	list[0] = listDim - 1;
	return TRUE;
}
/******************************************************************************
*
* dsTypeListEnter - parse type declaration and enter tid in type list
*
*/
int dsTypeListEnter(size_t *list, char *str, char **ptr)
{
	char *name;
	size_t h, tid;

	if (!dsTypeId(&tid, str, ptr)) {
		return FALSE;
	}
	name = dsTidInfo[tid].type->name;
	if (!dsTypeListFindName(&h, list, name, NULL)) {
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
* dsTypeListFindName - find entry with type name in str
*
*/
int dsTypeListFindName(size_t *pH, size_t *list, char *str, char **ptr)
{
	int c;
	size_t h, len, tid, n = list[0];

	if (ptr != NULL) {
		*ptr = str;
	}
	for (; isspace(*str); str++);
	for (h = len = 0; isalnum(c = str[len]) || c == '_'; len++) {
		h = ((h << 8) + c)%n;
	}
	for (h++; (tid = list[h]) != 0; h = h > 1 ? h - 1 : list[0]) {
		if (tid >= dsTidCount) {
			DS_ERROR(DS_E_INVALID_TYPE_ID);
		}
		if (dsNextName(str, &str, dsTidInfo[tid].type->name)) {
			break;
		}
		if (n-- == 0) {
			DS_ERROR(DS_E_TOO_MANY_TYPES);
		}
	}
	if (ptr != NULL) {
		*ptr = str;
	}
	*pH = h;
	return TRUE;
}
/******************************************************************************
*
* dsTypeListFindTid - find entry in type list with given tid
*
*/
int dsTypeListFindTid(size_t *pH, size_t *list, size_t tid)
{
	if (tid < 1 || tid >= dsTidCount) {
		DS_ERROR(DS_E_INVALID_TYPE_ID);
	}
	/* fastest way is to find type name */
	return dsTypeListFindName(pH, list, dsTidInfo[tid].type->name, NULL);
}
/******************************************************************************
*
* dsTypeListFree - free type list
*
*/
int  dsTypeListFree(size_t *list)
{
	dsDsetFree(list);
	return TRUE;
}
