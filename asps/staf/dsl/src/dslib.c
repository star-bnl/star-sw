/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dslib.c - routines  datasets */

/*
modification history
--------------------
30jul93,whg  written.
11feb95,whg  Added dsTasProject removed pUser from functions
*/

/*
DESCRIPTION
TBS ...
*/
#define DS_PRIVATE 
#include <stdlib.h>
#include <string.h>
#include "dscodes.h"
#include "dstype.h"

/******************************************************************************
*
* dsAddTable - add table to dataset
*
*/
int dsAddTable(DS_DATASET_T *pDataset, char *name,
	char *decl, size_t nRow, void *ppData)
{
	char **ptr = ppData;
	size_t n;
	DS_DATASET_T *pTable;

	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	if (pDataset->elcount >= pDataset->maxcount) {
		DS_ERROR(DS_E_DATASET_FULL);
	}
	pTable = &pDataset->p.child[pDataset->elcount];
	memset((char *)pTable, 0 , sizeof(DS_DATASET_T));

	if (!dsIdentifier(pTable->name, name, NULL)) {
		return FALSE;
	}
	for (n = pDataset->elcount; n-- > 0;) {
		if (strcmp(pTable->name, pDataset->p.child[n].name) == 0) {
			DS_ERROR(DS_E_DUPLICATE_TABLE_NAME);
		}
	}
	if (!dsTypeId(&pTable->tid, decl, NULL)) {
		return FALSE;
	}
	pTable->elcount = pTable->maxcount = nRow;
	pTable->parent = pDataset;
	if (ptr == NULL || *ptr == NULL) {
		if (!dsAllocTables(pTable)) {
			return FALSE;
		}
		if (ptr != NULL) {
			*ptr = pTable->p.data;
		}
	}
	else {
		pTable->p.data = *ptr;
	}
	pDataset->elcount++;
	return TRUE;
}
/******************************************************************************
*
* dsFindTable - find table in dataset and return pointer to descriptor
*
*/
int dsFindTable(DS_DATASET_T *pDataset, char *name,
	char *decl, DS_DATASET_T **ppTable)
{
	size_t tid;
	DS_DATASET_T *table = NULL;

	if (!dsFindDataset(&table, pDataset, name) ||
		table == NULL || !DS_IS_TABLE(table)) {
		DS_ERROR(DS_E_TABLE_NOT_FOUND);
	}
	if (decl != NULL) {
		if (!dsTypeId(&tid, decl, NULL)) {
			return FALSE;
		}
		if (table->tid != tid) {
			DS_ERROR(DS_E_TYPE_MISSMATCH);
		}
	}
	*ppTable = table;
	return TRUE;
}
/***********************************************************************
*
* dsMapTable - map dataset table to program variable
*
*/
int dsMapTable(DS_DATASET_T *pDataset, char *name,
        char *decl, size_t *pCount, void *ppData)
{
        char **ptr = ppData;
        DS_DATASET_T *table;

        if (!dsFindTable(pDataset, name, decl, &table)) {
                return FALSE;
        }
        if (*ptr == NULL) {
                *ptr = table->p.data;
        }
        else {
                if (table->elcount > *pCount) {
                        DS_ERROR(DS_E_TABLE_TOO_SMALL);
                }
                if ((table->flags & DS_ALLOC_P) != 0) {
                        dsDsetFree(table->p.data);
                        table->flags &= ~DS_ALLOC_P;
                }
                table->p.data = *ptr;
        }
        if (pCount != NULL) {
                *pCount = table->elcount;
        }
        return TRUE;
}
/******************************************************************************
*
* dsNewDataset - create an empty dataset with up to setDim -1 entries
*
*/
int dsNewDataset(DS_DATASET_T **ppDataset, char *name, size_t setDim)
{
	size_t size = setDim*sizeof(DS_DATASET_T);
	DS_DATASET_T *pDataset;

	if (setDim < 2) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if ((pDataset = *ppDataset) == NULL) {
		if ((pDataset = (DS_DATASET_T *)dsDsetAlloc(size)) == NULL) {
			return FALSE;
		}
	}
	memset((char *)pDataset, 0, sizeof(DS_DATASET_T));
	if (!dsIdentifier(pDataset->name, name, NULL)) {
		if (*ppDataset == NULL) {
			dsDsetFree(pDataset);
		}
		return FALSE;
	}
	pDataset->maxcount = setDim - 1;
	pDataset->p.child = &pDataset[1];
	if (*ppDataset == NULL) {
		pDataset->flags = DS_ALLOC_NODE;
		*ppDataset = pDataset;
	}
	return TRUE;
}
/******************************************************************************
*
* dsTasProject - project data to TAS variable
*
*/
int dsTasProject(DS_DATASET_T *pDataset, char *name,
        char *decl, size_t *pCount, void *ppData)
{
	char **ptr= ppData;
	int rtn;
	DS_DATASET_T *pSrcTable, table;

	memset (&table, 0, sizeof(DS_DATASET_T));
	table.maxcount = *pCount;
	*pCount = 0;
	if ((table.p.data = *ptr) == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	if (!dsFindDataset(&pSrcTable, pDataset, name) ||
		pSrcTable == NULL || !DS_IS_TABLE(pSrcTable)) {
		DS_ERROR(DS_E_TABLE_NOT_FOUND);
	}
	strcpy(table.name, pSrcTable->name);
	if (!dsTypeId(&table.tid, decl, NULL)) {
		return FALSE;
	}
	rtn = dsProjectTable(&table, pSrcTable);
	*pCount = table.elcount;
	return rtn;
}
