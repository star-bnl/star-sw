/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dstable.c - routines that act on table data */

/*
modification history
--------------------
26feb95,whg  collected from other files
*/

/*
DESCRIPTION
relation database operations for tables
*/
#define DS_PRIVATE
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "dstype.h"

/*****************************************************************************
*
* dsCmpData - compare data arrays
*
* RETURN:
*        1 fieldOne > fieldTwo
*        0 fieldOne == fieldTwo
*       -1 fieldOne < fieldTwo
*       -2 type error
*/
int dsCmpData(DS_TYPE_T *type, unsigned count, void *baseOne, void *baseTwo)
{
	int c;
	size_t i;
	DS_PTR_UNION_T p1, p2;
	DS_FIELD_T *field, *limit;

	p1.v = baseOne;
	p2.v = baseTwo;

	switch(type->code) {

		case DS_TYPE_CHAR:
			if ((c = strncmp(p1.c, p2.c, count)) != 0) {
				return c < 0 ? -1 : 1;
			}
			break;

		case DS_TYPE_OCTET:
			for (i = 0; i < count; i++) {
				if (p1.o[i] != p2.o[i]) {
					return p1.o[i] < p2.o[i] ? -1 : 1;
				}
			}
			break;

		case DS_TYPE_SHORT:
			for (i = 0; i < count; i++) {
				if (p1.s[i] != p2.s[i]) {
					return p1.s[i] < p2.s[i] ? -1 : 1;
				}
			}
			break;

		case DS_TYPE_U_SHORT:
			for (i = 0; i < count; i++) {
				if (p1.us[i] != p2.us[i]) {
					return p1.us[i] < p2.us[i] ? -1 : 1;
				}
			}
			break;

		case DS_TYPE_LONG:
			for (i = 0; i < count; i++) {
				if (p1.l[i] != p2.l[i]) {
					return p1.l[i] < p2.l[i] ? -1 : 1;
				}
			}
			break;

		case DS_TYPE_U_LONG:
			for (i = 0; i < count; i++) {
				if (p1.ul[i] != p2.ul[i]) {
					return p1.ul[i] < p2.ul[i] ? -1 : 1;
				}
			}
			break;

		case DS_TYPE_FLOAT:
			for (i = 0; i < count; i++) {
				if (p1.f[i] != p2.f[i]) {
					return p1.f[i] < p2.f[i] ? -1 : 1;
				}
			}
			break;

		case DS_TYPE_DOUBLE:
			for (i = 0; i < count; i++) {
				if (p1.d[i] != p2.d[i]) {
					return p1.d[i] < p2.d[i] ? -1 : 1;
				}
			}
			break;

		case DS_TYPE_STRUCT:
			limit = DS_FIELD_PTR(type) + type->nField;
			for (i = 0; i < count; i++) {
				for (field = DS_FIELD_PTR(type); field < limit; field++) {
					if ((c = dsCmpData(field->type, field->count,
						p1.c + field->offset, p2.c + field->offset)) != 0) {
						return c;
					}
				}
				p1.c += type->size;
				p2.c += type->size;
			}
			break;

		default:
			return -2;
	}
	return 0;
}
/*****************************************************************************
*
* dsCmpKeys - compare key data
*
* RETURN:
*        1 fieldOne > fieldTwo
*        0 fieldOne == fieldTwo
*       -1 fieldOne < fieldTwo
*       -2 type error
*/
int dsCmpKeys(char *baseOne, char *baseTwo, DS_KEY_T *key)
{
	int c;
	size_t i;
	
	for (i = 0; i < key->count; i++) {
		if ((c = dsCmpData(key->field[i][0]->type,
			key->field[i][0]->count,
			baseOne + key->field[i][0]->offset,
			baseTwo + key->field[i][1]->offset)) != 0) {
			return c;
		}
	}
	return 0;
}
/*****************************************************************************
*
* dsTasProject - project data to TAS variable
*
* RETURNS: TRUE if success else FALSE
*/
int dsTasProject(DS_DATASET_T *pDataset, const char *name,
	const char *typeSpecifier, size_t *pCount, void *ppData)
{
	int rtn;
	DS_DATASET_T *pSrcTable, table;

	memset (&table, 0, sizeof(DS_DATASET_T));
	table.maxcount = *pCount;
	*pCount = 0;
	if (!dsFindEntry(&pSrcTable, pDataset, name)) {
		return FALSE;
	}
	if (!DS_IS_TABLE(pSrcTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	if (pSrcTable->elcount == 0) {
		return TRUE;
	}
	if (ppData == NULL || (table.p.data = *(char **)ppData) == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	strcpy(table.name, pSrcTable->name);
	if (!dsTypeId(&table.tid, typeSpecifier, NULL)) {
		return FALSE;
	}
	rtn = dsProjectTable(&table, pSrcTable, NULL);
	*pCount = table.elcount;
	return rtn;
}
