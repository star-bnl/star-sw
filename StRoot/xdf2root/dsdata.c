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
							char *str, char **ptr, size_t *tList);
static int dsDatasetSpecifierR(DS_DATASET_T *dataset, DS_LIST_T *list,
							   size_t level, char **in, char *limit);
static int dsIsAcyclicR(DS_DATASET_T *dataset, DS_LIST_T *list,
						DS_LIST_T *path);
/******************************************************************************
*
* dsAddTable - add table to dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsAddTable(DS_DATASET_T *pDataset, char *name,
	char *typeSpecifier, size_t nRow, char **ppData)
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

	dsListInit(&list);
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
					 size_t *tList, char *str, char **ptr)
{
	DS_LIST_T list;

	dsListInit(&list);
	if (!dsCreateDatasetR(NULL, &list, str, &str, tList)) {
		goto fail;
	}
	*ppDataset = list.pItem[0];
	if (ptr != NULL) {
		*ptr = str;
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
							char *str, char **ptr, size_t *tList)
{
	char name[DS_NAME_DIM];
	int c;
	size_t h, n;
	DS_DATASET_T *child;

	if(!dsParseName(name, str, &str)) {
		if (dsNonSpace(str, &str) != '&' || !dsParseNumber(&n, str, &str)) {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
		if (--n >= list->count) {
			DS_ERROR(DS_E_INVALID_LINK);
		}
		child = list->pItem[n];
		if (!dsLink(parent, child)) {
			return FALSE;
		}
		*ptr = str;
		return TRUE;
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
	if ((c = dsNonSpace(str, &str)) == '(') {
		if (!dsParseName(name, str, &str)) {
			DS_ERROR(DS_E_INVALID_TYPE_NAME);
		}
		if (!dsTypeListFind(&h, tList, name)) {
			return FALSE;
		}
		if ((child->tid = tList[h]) == 0) {
			DS_ERROR(DS_E_UNDEFINED_TYPE);
		}
		if ((c = dsNonSpace(str, &str)) == ',') {
			if (!dsParseNumber(&child->maxcount, str, &str)) {
				DS_ERROR(DS_E_SYNTAX_ERROR);
			}
			c = dsNonSpace(str, &str);
		}
		if ( c != ')') {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
		*ptr = str;
		return TRUE;		
	}
	if (c != '{') {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	if ((c = dsNonSpace(str, &str)) != '}') {
		str--;
		for (;;) {
			if (!dsCreateDatasetR(child, list, str, &str, tList)) {
				return FALSE;
			}
			if ((c = dsNonSpace(str, &str)) == '}') {
				break;
			}
			if (c != ',') {
				DS_ERROR(DS_E_SYNTAX_ERROR);
			}
		}
	}
	*ptr = str;
	return TRUE;		
}
/******************************************************************************
*
* dsDatasetSpecifier - format a declaration for a dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsDatasetSpecifier(DS_DATASET_T *pDataset, char *buf, size_t bufSize)
{
	char *in = buf, *limit = buf + bufSize;
	DS_LIST_T list;

	dsListInit(&list);
	if (!dsPutStr("data ", &in, limit)) {
		DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
	}
	if (!dsDatasetSpecifierR(pDataset, &list, 0, &in, limit)) {
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
static int dsDatasetSpecifierR(DS_DATASET_T *pDataset, DS_LIST_T *list,
							   size_t level, char **in, char *limit)
{
	size_t i;
	DS_TYPE_T *type;

	if (!dsPutTabs(level, in, limit)) {
		DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
	}
	if (dsVisited(list, pDataset)) {
		if (!dsPutStr("&", in, limit)||
			!dsPutNumber(pDataset->visit, in, limit)) {
			DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
		}
		return TRUE;
	}
	if (!dsMark(list, pDataset)) {
		return FALSE;
	}
	if (!dsPutStr(pDataset->name, in, limit)) {
		DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
	}
	if (DS_IS_TABLE(pDataset)) {
		if (!dsTypePtr(&type, pDataset->tid)) {
			return FALSE;
		}
		if (!dsPutStr("(", in, limit)||
			!dsPutStr(type->name, in, limit)||
			!dsPutStr(", ", in, limit) < 0 ||
			!dsPutNumber(pDataset->elcount, in, limit)||
			!dsPutStr(")", in, limit)) {
			DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
		}
		return TRUE;
	}
	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	if (!dsPutStr("{\n", in, limit)) {
		DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
	}
	if (!dsCheckDupEntry(pDataset)) {
		return FALSE;
	}
	for (i = 0; i < pDataset->elcount; i++) {
		if (i != 0 && !dsPutStr(",\n", in, limit)) {
			DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
		}
		if (!dsDatasetSpecifierR(pDataset->p.link[i],
			list, level + 1, in, limit)) {
			return FALSE;
		}
	}
	if (!dsPutStr("\n", in, limit) ||
		!dsPutTabs(level, in, limit) ||
		!dsPutStr("}", in, limit)) {
		DS_ERROR(DS_E_TYPE_STRING_TOO_LONG);
	}
	return TRUE;
}
/*****************************************************************************
*
* dsInitTable - initialize a table descriptor
*
* RETURNS: TRUE if success else FALSE
*/
int dsInitTable(DS_DATASET_T *pTable, char *tableName,
	char *typeSpecifier, unsigned rowCount, void *pData)
{
	char *ptr;
	if (pTable == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	memset((char *)pTable, 0 , sizeof(DS_DATASET_T));
	if (!dsParseName(pTable->name, tableName, &ptr) || *ptr != '\0') {
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

	dsListInit(&list);
	dsListInit(&path);
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
		dsListInit(&list);
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
