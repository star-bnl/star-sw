/* Copyright 1993, Lawrence Berkeley Laboratory */

/* c.dstid - routines to define data type IDs */

/*
modification history
--------------------
24apr93,whg  written.
28feb95,whg  change to CORBA IDL
11jun96,whg  added indirection to dataset structure
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
	char *specifier;		/* string for tid type specifier */
	int len;		/* length of specifier string */
}TID_INFO_T;

static size_t dsTidCount = 0;	/* index of next dsTidInfo element */
static int dsTidHit = 0;
static int dsTidMiss = 0;
static int dsTidSlots = 0;
static TID_HASH_T **dsTidHash = NULL;
static TID_INFO_T *dsTidInfo = NULL;
/******************************************************************************
*
* dsTidInit - intilize dsTidHash and dsTidInfo structs
* 
* RETURNS: TRUE if success else FALSE
*/
static int dsTidInit()
{
	size_t hashSize = DS_TID_HASH_LEN*sizeof(TID_HASH_T *);
	size_t infoSize = (DS_MAX_TID + 1)*sizeof(TID_INFO_T);

	/********* start critical section *********/
	if (!dsTypeSemTake()) {
		return FALSE;
	}
	if (dsTidCount != 0) {
		/* exit if initialized */
		return dsTypeSemGive();
	}
	if ((dsTidHash = dsTypeCalloc(hashSize)) != NULL) {
		if ((dsTidInfo = dsTypeCalloc(infoSize)) == NULL) {
			dsTypeFree(dsTidHash, hashSize);
			dsTidHash = NULL;
		}
	}
	if (dsTidHash == NULL) {
		dsTypeSemGive();
		return FALSE;
	}
	/* set dsTidCount to indicate initilized */
	dsTidCount = 1;
	return dsTypeSemGive();
	/********** end critical section **********/
}
/******************************************************************************
*
* dsTypeSpecifier - get pointer and length of specifier string for a tid
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeSpecifier(const char **ptr, size_t *pLen, size_t tid)
{
	char buf[DS_MAX_SPEC_LEN+1], *str;
	size_t len;

	if (tid < 1 || tid >= dsTidCount) {
		DS_ERROR(DS_E_INVALID_TYPE_ID);
	}
	/* check if string is already formated */
	if (dsTidInfo[tid].specifier == NULL) {
		if (!dsFormatTypeSpecifier(buf, sizeof(buf), dsTidInfo[tid].type)) {
			return FALSE;
		}
		len = strlen(buf);
		if ((str = dsTypeCalloc(len + 1)) == NULL) {
			return FALSE;
		}
		strcpy(str, buf);

		/******** start critical section *********/
		if (!dsTypeSemTake()) {
			goto fail;
		}
		/* check for string formatted by another thread */
		if (dsTidInfo[tid].specifier == NULL) {
			dsTidInfo[tid].specifier = str;
			dsTidInfo[tid].len = len;
			str = NULL;
		}
		if (!dsTypeSemGive()) {
			goto fail;
		}
		/********* end critical section **********/
		if (str != NULL) {
			/* free memory - specifier was created by another thread */
			dsTypeFree(str, len + 1);
		}
	}
	if (ptr != NULL) {
		*ptr = dsTidInfo[tid].specifier;
	}
	if (pLen != NULL) {
		*pLen = dsTidInfo[tid].len;
	}
	return TRUE;

fail:
	if (str != NULL) {
		dsTypeFree(str, len + 1);
	}
	return FALSE;
}
/******************************************************************************
*
* dsTypePtr - get pointer to a type structure for a tid
*
* RETURNS: TRUE if success else FALSE
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
* RETURNS: TRUE
*/
int dsTidHashStats()
{
	printf("tidHashStats: tidSlots %d, tidHit %d, tidMiss %d\n",
		dsTidSlots, dsTidHit, dsTidMiss);
	return TRUE;
}
/******************************************************************************
*
* dsTypeId - get tid for declaration string
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeId(size_t *pTid, const char *str, const char **ptr)
{
	const char *next;
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
if(dsTidHash[h] == NULL)dsTidSlots++;
	type = NULL;
	if (!dsParseType(&type, &typeSize, str, &next)) {
		return FALSE;
	}
	n = next - str;

	/* create entry for open hash */
	newSize = sizeof(TID_HASH_T) + n;
	if ((new = dsTypeCalloc(newSize)) == NULL) {
		dsTypeFree(type, typeSize);
		return FALSE;
	}
	strncpy(new->str, str, n); 
	new->str[ n]=0; /* hjw 19Feb98 */
	new->len = n;
	tptr = (TID_HASH_T *)&dsTidHash[h];

	for(entry = NULL;;) {
		/* look for type match in hash table */
		while (new->tid == 0 && tptr->next != NULL) {
			tptr = tptr->next;
			if (dsTypeCmp(type, dsTidInfo[tptr->tid].type) == 0) {
				new->tid = tptr->tid;
				dsTypeFree(type, typeSize);
				type = NULL;
			}
		}
		/********* start critical section *********/
		if( !dsTypeSemTake()) {
			goto fail;
		}
		/* check if entries were created by other threads */
		if (sptr->next == NULL) {
			/* create a new tid if type not found in hash table */
			if (new->tid == 0) {
				if (dsTidCount >= DS_MAX_TID) {
					/* exit with error - dsTidInfo full */
					if (!dsTypeSemGive()) {
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
			if (!dsTypeSemGive())  {
				goto fail;
			}
			entry = new;
			break;
		}
		if (!dsTypeSemGive()) {
			goto fail;
		} 
		/********** end critical section **********/

		/* check entry created by other thread */
		sptr = sptr->next;
		if (strncmp(str, sptr->str, sptr->len) == 0) {
			dsTypeFree(new, newSize);
			if (type) {
				dsTypeFree(type, typeSize);
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
		dsTypeFree(type, typeSize);
	}
	if (new != NULL) {
		dsTypeFree(new, newSize);
	}
	return FALSE;
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
int dsTypeListEnter(size_t *list, const char *str, const char **ptr)
{
	char *name;
	size_t h, tid;

	if (!dsTypeId(&tid, str, ptr)) {
		return FALSE;
	}
	name = dsTidInfo[tid].type->name;
	if (!dsTypeListFind(&h, list, name)) {
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

	for (; isspace(*str); str++);
	for (h = len = 0; isalnum(c = str[len]) || c == '_'; len++) {
		h = ((h << 8) + c)%n;
	}
	for (h++; (tid = list[h]) != 0; h = h > 1 ? h - 1 : list[0]) {
		if (tid >= dsTidCount) {
			DS_ERROR(DS_E_INVALID_TYPE_ID);
		}
		if ((c = dsCmpName(str, dsTidInfo[tid].type->name)) <= 0) {
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
