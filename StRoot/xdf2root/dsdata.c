/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsdata.c - routines process dataset definitions */

/*
modification history
--------------------
26apr93,whg  written.
15feb95,whg  change to CORBA IDL
28May96,whg  new ds_dataset_t structure
*/
/*
DESCRIPTION
routines to parse and format dataset specifiers
and manage dataset structures
*/
#include <string.h>
#define DS_PRIVATE
#include "dstype.h"

static int dsCreateDatasetR(DS_DATASET_T *parent, DS_LIST_T *list,
							DS_BUF_T *bp, size_t *tList);
static int dsDatasetSpecifierR(DS_BUF_T *bp, DS_DATASET_T *dataset,
							   DS_LIST_T *list, size_t level);
static int dsIsAcyclicR(DS_DATASET_T *dataset, DS_LIST_T *list,
						DS_LIST_T *path);
/******************************************************************************
*
* dsAddTable - add table to dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsAddTable(DS_DATASET_T *pDataset, const char *name,
	const char *typeSpecifier, size_t nRow, char **ppData)
{
	char *pData;
	DS_DATASET_T *pTable;

	pData = ppData == NULL ? NULL : *ppData;
	if (!dsNewTable(&pTable, name, typeSpecifier, nRow, pData)) {
		return FALSE;
	}
	if (pTable->p.data == NULL && nRow != 0) {
		if (!dsAllocTables(pTable)) {
			goto fail;
		}
		if (ppData != NULL) {
			*ppData = pTable->p.data;
		}
	}
	if (!dsLink(pDataset, pTable)) {
		goto fail;
	}
	return TRUE;
fail:
	dsFreeDataset(pTable);
	return FALSE;
}
/******************************************************************************
*
* dsAllocTables - allocate memory for dataset tables
*
* RETURNS: TRUE if success else FALSE
*/
int dsAllocTables(DS_DATASET_T *dataset)
{
	size_t i;
	DS_LIST_T list;
	DS_DATASET_T *item;

	if (!dsListInit(&list)) {
		return FALSE;
	}
	if (!dsVisitList(&list, dataset)) {
		goto fail;
	}
	for (i = 0; i < list.count; i++) {
		item = list.pItem[i];
		if (DS_IS_TABLE(item) && item->p.data == NULL
			&& !dsReallocTable(item, item->maxcount)) {
			goto fail;
		}
	}
	dsListFree(&list);
	return TRUE;
fail:
	dsListFree(&list);
	return FALSE;
}
/******************************************************************************
*
* dsCheckDupEntry - verify entry names are unique 
*
* RETURNS: TRUE if unique names, FALSE if duplicate or names collide
*/
static int dsCheckDupEntry(DS_DATASET_T *pDataset)
{
	int c, n;
	DS_DATASET_T **p1, **p2, **last;

	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	if ((n = pDataset->elcount - 1) > 0) {
		for (p1 = pDataset->p.link, last = p1 + n; p1 < last; p1++) {
			for (p2 = p1 + 1; p2 <= last; p2++) {
				if ((c = dsCmpName((*p1)->name, (*p2)->name)) <= 0) {
					if (c < 0) {
						DS_ERROR(DS_E_NAMES_COLLIDE);
					}
					else {
						DS_ERROR(DS_E_DUPLICATE_NAME);
					}
				}
			}
		}
	}
	return TRUE;
}
/******************************************************************************
*
* dsCreateDataset - create a dataset from its declaration string
*
* RETURNS: TRUE if success else FALSE
*/
int dsCreateDataset(DS_DATASET_T **ppDataset,
					 size_t *tList, const char *str, const char **ptr)
{
	DS_BUF_T bp;
	DS_LIST_T list;

	if (ptr != NULL) {
		*ptr = str;
	}
	if (!dsListInit(&list)) {
		return FALSE;
	}
	DS_GET_INIT(&bp, str);
	if (!dsCreateDatasetR(NULL, &list, &bp, tList)) {
		goto fail;
	}
	*ppDataset = list.pItem[0];
	if (ptr != NULL) {
		*ptr = bp.out;
	}
	return dsListFree(&list);
fail:
	if (list.count > 0) {
		dsFreeDataset(list.pItem[0]);
	}
	dsListFree(&list);
	return FALSE;
}
/******************************************************************************
*
* dsCreateDatasetR - recursive part of dsCreateDataset
*
* RETURNS: TRUE if success else FALSE
*/
static int dsCreateDatasetR(DS_DATASET_T *parent, DS_LIST_T *list,
							DS_BUF_T *bp, size_t *tList)
{
	char name[DS_NAME_DIM];
	int c;
	size_t h, n;
	DS_DATASET_T *child;

	if(dsGetName(name, bp) < 0) {
		if	(dsGetNonSpace(bp) != '&' || dsGetNumber(&n, bp) < 0) {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
		if (--n >= list->count) {
			DS_ERROR(DS_E_INVALID_LINK);
		}
		child = list->pItem[n];
		return dsLink(parent, child);
	}
	if (!dsNewDataset(&child, name)) {
			return FALSE;
	}
	if (!dsListAppend(list, child)) {
		dsFreeDataset(child);
		return FALSE;
	}
	if (parent != NULL && !dsLink(parent, child)) {
		dsFreeDataset(child);
		return FALSE;
	}
	if ((c = dsGetNonSpace(bp)) == '(') {
		if (dsGetName(name, bp) < 0) {
			DS_ERROR(DS_E_INVALID_TYPE_NAME);
		}
		if (!dsTypeListFind(&h, tList, name)) {
			return FALSE;
		}
		if ((child->tid = tList[h]) == 0) {
			DS_ERROR(DS_E_UNDEFINED_TYPE);
		}
		if ((c = dsGetNonSpace(bp)) == ',') {
			if (dsGetNumber(&child->maxcount, bp) < 0) {
				DS_ERROR(DS_E_SYNTAX_ERROR);
			}
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
	if ((c = dsGetNonSpace(bp)) != '}') {
		dsUngetc(c, bp);
		for (;;) {
			if (!dsCreateDatasetR(child, list, bp, tList)) {
				return FALSE;
			}
			if ((c = dsGetNonSpace(bp)) == '}') {
				break;
			}
			if (c != ',') {
				DS_ERROR(DS_E_SYNTAX_ERROR);
			}
		}
	}
	return TRUE;		
}
/******************************************************************************
*
* dsDatasetSpecifier - format a declaration for a dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsDatasetSpecifier(DS_BUF_T *bp, DS_DATASET_T *pDataset)
{
	DS_LIST_T list;

	if (!dsListInit(&list)) {
		return FALSE;
	}
	if (dsPuts("data ", bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (!dsDatasetSpecifierR(bp, pDataset, &list, 0)) {
		goto fail;
	}
	if (dsPutc('\0', bp) < 0) {
		DS_LOG_ERROR(DS_E_ARRAY_TOO_SMALL);
		goto fail;
	}
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
}
/******************************************************************************
*
* dsDatasetSpecifierR - recursive part to format a dataset declaration
*
* RETURNS: TRUE if success else FALSE
*/
static int dsDatasetSpecifierR(DS_BUF_T *bp, DS_DATASET_T *pDataset,
							   DS_LIST_T *list, size_t level)
{
	size_t i;
	DS_TYPE_T *type;

	if (dsPutTabs(level, bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (dsVisited(list, pDataset)) {
		if (dsPutc('&', bp) < 0 || dsPutNumber(pDataset->visit, bp) < 0) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}
		return TRUE;
	}
	if (!dsMark(list, pDataset)) {
		return FALSE;
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
	if (!dsCheckDupEntry(pDataset)) {
		return FALSE;
	}
	for (i = 0; i < pDataset->elcount; i++) {
		if (i != 0 && dsPuts(",\n", bp) < 0) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}
		if (!dsDatasetSpecifierR(bp,
			pDataset->p.link[i], list, level + 1)) {
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
/*****************************************************************************
*
* dsInitTable - initialize a table descriptor
*
* RETURNS: TRUE if success else FALSE
*/
int dsInitTable(DS_DATASET_T *pTable, const char *tableName,
	const char *typeSpecifier, unsigned rowCount, void *pData)
{
	if (pTable == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	memset((char *)pTable, 0 , sizeof(DS_DATASET_T));
	if (!dsCopyName(pTable->name, tableName, NULL)) {
		DS_ERROR(DS_E_INVALID_TABLE_NAME);
	}		
	if (!dsTypeId(&pTable->tid, typeSpecifier, NULL)) {
		return FALSE;
	}
	pTable->maxcount = rowCount;
	if ((pTable->p.data = pData) != NULL) {
		pTable->elcount = rowCount;
	}
	return TRUE;
}
/*****************************************************************************
*
* dsIsAcyclic - check for acyclic dataset
*
* RETURN TRUE if acyclic else FALSE
*/
int dsIsAcyclic(DS_DATASET_T *dataset)
{
	int status;
	DS_LIST_T list, path;

	if (!dsListInit(&list) || !dsListInit(&path)) {
		return FALSE;
	}
	status = dsIsAcyclicR(dataset, &list, &path);
	if (!dsListFree(&list) || !dsListFree(&path)){
		return FALSE;
	}
	return status;
}
/*****************************************************************************
*
* dsIsAcyclicR - recursive part of dsIsAcyclic
*
* RETURN TRUE if acyclic else FALSE
*/
static int dsIsAcyclicR(DS_DATASET_T *dataset, DS_LIST_T *list, DS_LIST_T *path)
{
	size_t i;

	if (DS_IS_DATASET(dataset)) {
		if (!dsVisited(list, dataset)) {
			if (!dsMark(list, dataset) ||
				!dsListAppend(path, dataset)) {
				return FALSE;
			}
			for (i = 0; i < dataset->elcount; i++) {
				if (!dsIsAcyclicR(dataset->p.link[i], list, path)) {
					return FALSE;
				}
			}
			path->count--;
		}
		else {
			for (i = 0; i < path->count; i++) {
				if (path->pItem[i] == dataset) {
					DS_ERROR(DS_E_DATASET_HAS_CYCLE);
				}
			}
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsLink - link child to parent
*
* RETURNS: TRUE if success else FALSE
*/
int dsLink(DS_DATASET_T *parent, DS_DATASET_T *child)
{
	int c;
	size_t i;

	if (!DS_IS_DATASET(parent) || !DS_IS_VALID(child)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	for (i = 0; i < parent->elcount; i++) {
		if ((c = dsCmpName(child->name, parent->p.link[i]->name)) <= 0) {
			if (c < 0) {
				DS_ERROR(DS_E_NAMES_COLLIDE);
			}
			else {
				DS_ERROR(DS_E_DUPLICATE_NAME);
			}
		}
	}
	if (parent->elcount >= parent->maxcount) {
		if (!dsRealloc(parent, parent->elcount + DS_ALLOC_INC)) {
			return FALSE;
		}
	}
	parent->p.link[parent->elcount++] = child;
	child->refcount++;
	return TRUE;
}
/*****************************************************************************
*
* dsLink - link child to parent. error if a cycle will be created
*
* RETURNS: TRUE if success else FALSE
*/
int dsLinkAcyclic(DS_DATASET_T *parent, DS_DATASET_T *child)
{
	size_t i, n;
	DS_LIST_T list;

	if (DS_IS_DATASET(child)) {
		if (!dsListInit(&list)) {
			return FALSE;
		}
		if (!dsVisitList(&list, child)) {
			dsListFree(&list);
			return FALSE;
		}
		for (n = list.count, i = 0; i < n && list.pItem[i] != parent; i++);
		if (!dsListFree(&list)) {
			return FALSE;
		}
		if (i != n) {
			DS_ERROR(DS_E_DATASET_WOULD_BE_CYCLIC);
		}
	}
	return dsLink(parent, child);
}
/*****************************************************************************
*
* dsUnlink - remove an entry from a dataset
*
* RETURNS: TRUE if valid else FALSE
*/
int dsUnlink(DS_DATASET_T *parent, DS_DATASET_T *child)
{
	size_t i;

	if (!DS_IS_DATASET(parent)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	for (i = 0; i < parent->elcount ; i++) {
		if (child == parent->p.link[i]) {
			break;
		}
	}
	if (i == parent->elcount) {
		DS_ERROR(DS_E_ENTRY_NOT_FOUND);
	}
	if (child == NULL || child->refcount < 1) {
		DS_ERROR(DS_E_REFCOUNT_ERROR);
	}
	for (parent->elcount--; i < parent->elcount; i++) {
		parent->p.link[i] = parent->p.link[i + 1];
	}
	child->refcount--;
	return TRUE;
}
