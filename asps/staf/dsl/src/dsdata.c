/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsdata.c - routines process dataset definitions */

/*
modification history
--------------------
26apr93,whg  written.
15feb95,whg  change to CORBA IDL
*/

/*
DESCRIPTION
routines to parse and format dataset specifiers
and manage dataset structures
*/
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#include "dstype.h"

static int dsCreateDatasetR(DS_DATASET_T *pDataset, DS_BUF_T *bp,
	DS_MEM_T *skel, size_t *tList);
static int dsDatasetSpecifierR(DS_BUF_T *bp, DS_DATASET_T *pDataset, int level);
/******************************************************************************
*
* dsAllocTables - allocate memory for dataset tables
*
* RETURNS: TRUE if success else FALSE
*/
int dsAllocTables(DS_DATASET_T *pDataset)
{
	size_t i;

	if (DS_IS_DATASET(pDataset)) {
		for (i = 0; i < pDataset->elcount; i++) {
			if (!dsAllocTables(&pDataset->p.child[i])) {
				return FALSE;
			}
		}
		return TRUE;
	}
	if (!DS_IS_TABLE(pDataset)) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	if (pDataset->p.data != NULL) {
		return TRUE;
	}
	if (!dsReallocTable(pDataset, pDataset->maxcount)) {
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* dsCreateDataset - create a dataset structure from its declaration string
*
* RETURNS: TRUE if success else FALSE
*/
int dsCreateDataset(DS_DATASET_T **ppDataset,
	size_t dim, size_t *tList, char *str, char **ptr)
{
	int map[DS_MAX_DATASET], nDataSet = 0, nField = 0;
	size_t n, size;
	DS_BUF_T bp;
	DS_DATASET_T *pDataset;
	DS_MEM_T skel;

	if (ptr != NULL) {
		*ptr = str;
	}
	if (!dsFirstPass(&nField, map, &nDataSet, DS_MAX_DATASET, str)) {
		return FALSE;	
	}
	n = nDataSet + nField + 1;
	size = n*sizeof(DS_DATASET_T);
	if (*ppDataset != NULL) {
		if (dim < n) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}
		skel.first = (char *)*ppDataset;
	}
	else {
		if ((skel.first = dsDsetAlloc(size)) == NULL) {
			return FALSE;
		}
	}
	memset(skel.first, 0, size);
	pDataset = (DS_DATASET_T *)skel.first;
	skel.next = skel.first + sizeof(DS_DATASET_T);
	skel.limit = skel.first + size;
	skel.map = map;
	DS_GET_INIT(&bp, str);
	if (!dsCreateDatasetR(pDataset, &bp, &skel, tList)) {
		goto fail;
	}
	if (skel.limit != skel.next) {
		DS_LOG_ERROR(DS_E_SYSTEM_ERROR);
		goto fail;
	}
	if (*ppDataset == NULL) {
		pDataset->flags |= DS_ALLOC_NODE;
		*ppDataset = pDataset;
	}
	if (ptr != NULL) {
		*ptr = bp.out;
	}
	return TRUE;
fail:
	/* free malloced memory */
	if (*ppDataset == NULL) {
		dsDsetFree(skel.first);
	}
	return FALSE;
}
/******************************************************************************
*
* dsCreateDatasetR - second pass to parse dataset declaration
*
* RETURNS: TRUE if success else FALSE
*/
static int dsCreateDatasetR(DS_DATASET_T *pDataset, DS_BUF_T *bp,
	DS_MEM_T *skel, size_t *tList)
{
	char name[DS_NAME_DIM];
	int c;
	size_t h, i, size;
	unsigned nRow;

	if(dsGetName(pDataset->name, bp) < 0) {
		DS_ERROR(DS_E_INVALID_NAME);
	}
	if ((c = dsGetNonSpace(bp)) == '(') {
		if (dsGetName(name, bp) < 0) {
			DS_ERROR(DS_E_INVALID_TYPE_NAME);
		}
		if (!dsTypeListFind(&h, tList, name)) {
			return FALSE;
		}
		if ((pDataset->tid = tList[h]) == 0) {
			DS_ERROR(DS_E_UNDEFINED_TYPE);
		}
		if ((c = dsGetNonSpace(bp)) == ',') {
			if (dsGetNumber(&nRow, bp) < 0) {
				DS_ERROR(DS_E_SYNTAX_ERROR);
			}
			pDataset->maxcount = (size_t)nRow;
			c = dsGetNonSpace(bp);
		}
		if ( c != ')') {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
		return TRUE;		
	}
	if (c != '{') {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	pDataset->p.child = (DS_DATASET_T *)skel->next;
	pDataset->elcount = pDataset->maxcount = *skel->map++ + 1;
	size = pDataset->maxcount*sizeof(DS_DATASET_T);
	skel->next += size;
	if (skel->next > skel->limit) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	/* the next code allows an empty dataset */
	/* an empty node is allocated due to first pass problem */
	if ((c = dsGetNonSpace(bp)) == '}') {
		if (pDataset->elcount != 1) {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
		pDataset->elcount = 0;
		return TRUE;
	}
	dsUngetc(c, bp);		
	for (i = 0; i < pDataset->elcount; i++) {
		if (!dsCreateDatasetR(&pDataset->p.child[i], bp, skel, tList)) {
			return FALSE;
		}
		c = (i + 1) == pDataset->elcount ? '}' : ',';
		if (dsGetNonSpace(bp) != c) {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
	}
	if (!dsCheckDuplicate(pDataset->p.child->name,
		pDataset->elcount, sizeof(DS_DATASET_T))) {
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* dsDatasetSpecifier - format a declaration for a dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsDatasetSpecifier(char *str, size_t maxSize, DS_DATASET_T *pDataset)
{
	DS_BUF_T bp;
	
	DS_PUT_INIT(&bp, str, maxSize);
	if (dsPuts("data ", &bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (!dsDatasetSpecifierR(&bp, pDataset, 0)) {
		return FALSE;
	}
	if (dsPutc('\0', &bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	return TRUE;
}
/******************************************************************************
*
* dsDatasetSpecifierR - recursive part to format a dataset declaration
*
* RETURNS: TRUE if success else FALSE
*/
static int dsDatasetSpecifierR(DS_BUF_T *bp, DS_DATASET_T *pDataset, int level)
{
	size_t i;
	DS_TYPE_T *type;

	if (dsPutTabs(level, bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (dsPuts(pDataset->name, bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (DS_IS_TABLE(pDataset)) {
		if (!dsTypePtr(&type, pDataset->tid)) {
			return FALSE;
		}
		if (dsPutc('(', bp) < 0 ||
			dsPuts(type->name, bp) < 0 ||
			dsPuts(", ", bp) < 0 ||
			dsPutNumber(pDataset->elcount, bp) < 0 ||
			dsPutc(')', bp) < 0) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}
		return TRUE;
	}
	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	if (dsPuts("{\n", bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (!dsCheckDuplicate(pDataset->p.child->name, pDataset->elcount,
		sizeof(DS_DATASET_T))) {
		return FALSE;
	}
	for (i = 0; i < pDataset->elcount; i++) {
		if (i != 0 && dsPuts(",\n", bp) < 0) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}
		if (!dsDatasetSpecifierR(bp,
			&pDataset->p.child[i], level + 1)) {
			return FALSE;
		}
	}
	if (dsPutc('\n', bp) < 0 ||
		dsPutTabs(level, bp) < 0 ||
		dsPutc('}', bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	return TRUE;
}
/******************************************************************************
*
* dsFreeDataset - free memory for dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsFreeDataset(DS_DATASET_T *pDataset)
{
	size_t i;

	if (pDataset == NULL) {
		return TRUE;
	}
	if (DS_IS_DATASET(pDataset)) {
		for (i = 0; i < pDataset->elcount; i++) {
			dsFreeDataset(&pDataset->p.child[i]);
		}
	}
	else if (!DS_IS_TABLE(pDataset)) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	if (pDataset->flags & DS_ALLOC_P) {
		dsDsetFree(pDataset->p.data);
		pDataset->p.data = NULL;
		pDataset->flags &= ~DS_ALLOC_P;
	}
	if (pDataset->flags & DS_ALLOC_NODE) {
		dsDsetFree(pDataset);
	}
	return TRUE;
}
/*****************************************************************************
*
* dsReallocTable - allocate memory for table
*
* RETURNS: TRUE if success else FALSE
*/
int dsReallocTable(DS_DATASET_T *pTable, size_t nRow)
{
	char *ptr;
	size_t size;
	DS_TYPE_T *type;
	
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	if (!DS_DYNAMIC_TABLE(pTable) || nRow < pTable->elcount) {
		DS_ERROR(DS_E_REALLOC_TABLE_ERROR);
	}
	if (!dsTypePtr(&type, pTable->tid)) {
		return FALSE;
	}
	size = nRow*type->size;
	if (size == 0) {
		dsDsetFree(pTable->p.data);
		pTable->p.data = NULL;
		pTable->flags &= ~DS_ALLOC_P;
	}
	else {
		if ((ptr = dsDsetRealloc(pTable->p.data, size)) == NULL) {
			return FALSE;
		}
		pTable->flags |= DS_ALLOC_P;
		pTable->p.data = ptr;
	}		
	pTable->maxcount = nRow;
	return TRUE;
}
/*****************************************************************************
*
* dsValidDataset - check dataset for valid values
*
* RETURNS: TRUE if valid else FALSE
*/
 int dsValidDataset(DS_DATASET_T *pDset)
 {
 	if (pDset != NULL &&
 		(pDset->flags == (pDset->flags & DS_DATASET_FLAGS)) &&
 		(pDset->p.data != NULL ? pDset->elcount <= pDset->maxcount: 
 		pDset->elcount == 0 && (pDset->flags & DS_ALLOC_P) == 0)) {
 		return TRUE;
 	} 	
	if (pDset == NULL) {
		dsErrorPrint("dsValidDataset: NULL_POINTER_ERROR\n");
		return FALSE;
	}	
	dsErrorPrint("dsValidDataset: INVALID - ");
	dsErrorPrint("%s, flags %d, elcount %d, maxcount %d,",
		pDset->name, pDset->flags, pDset->elcount, pDset->maxcount);
	dsErrorPrint(" ptr %s NULL\n", pDset->p.data ? "!=" : "==");
	return FALSE;
}
