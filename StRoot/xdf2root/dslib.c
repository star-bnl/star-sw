/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dslib.c - routines  datasets */

/*
modification history
--------------------
30jul93,whg  written.
11feb95,whg  removed pUser from functions
*/

/*
DESCRIPTION
interface routines for definition of datasets and mapping
tables to program variables 
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
* RETURNS: TRUE if success else FALSE
*/
int dsAddTable(DS_DATASET_T *pDataset, char *name,
	char *typeSpecifier, size_t nRow, void *ppData)
{
	char *pData;
	size_t n;
	DS_DATASET_T *pTable;

	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	if (pDataset->elcount >= pDataset->maxcount) {
		DS_ERROR(DS_E_DATASET_FULL);
	}
	pTable = &pDataset->p.child[pDataset->elcount];
	if ((pData = ppData) != NULL) {
		pData = *(char **)ppData;
	}
	if (!dsNewTable(&pTable, name, typeSpecifier, nRow, pData)) {
		return FALSE;
	}
	for (n = pDataset->elcount; n-- > 0;) {
		if (dsCmpName(pTable->name, pDataset->p.child[n].name) <= 0) {
			DS_ERROR(DS_E_DUPLICATE_TABLE_NAME);
		}
	}
	if (pTable->p.data == NULL && nRow != 0) {
		if (!dsAllocTables(pTable)) {
			return FALSE;
		}
		if (ppData != NULL) {
			*(char **)ppData = pTable->p.data;
		}
	}
	pDataset->elcount++;
	return TRUE;
}
/*****************************************************************************
*
* dsFindTable - find table in dataset and return pointer to descriptor
*
* RETURNS: TRUE if success else FALSE
*/
int dsFindTable(DS_DATASET_T **ppTable,
	DS_DATASET_T *pDataset, char *name, char *typeSpecifier)
{
	int c;
	size_t i, tid;
	DS_DATASET_T *table = NULL;

	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_DATASET_REQUIRED);
	}
	for (i = 0; i < pDataset->elcount; i++) {
		if ((c = dsCmpName(pDataset->p.child[i].name, name)) <= 0) {
			if (c < 0) {
				DS_ERROR(DS_E_NAMES_COLLIDE);
			}
			table = &pDataset->p.child[i];
			break;
		}
	}
	if (table == NULL || !DS_IS_TABLE(table)) {
		DS_ERROR(DS_E_TABLE_NOT_FOUND);
	}
	if (typeSpecifier != NULL) {
		if (!dsTypeId(&tid, typeSpecifier, NULL)) {
			return FALSE;
		}
		if (table->tid != tid) {
			DS_ERROR(DS_E_TYPE_MISSMATCH);
		}
	}
	*ppTable = table;
	return TRUE;
}
/*****************************************************************************
*
* dsGetDataPtr - return pointer to table data
*
* RETURNS: TRUE if success else FALSE
*/
int dsGetDataPtr(void *ppData, DS_DATASET_T *pTable)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	*((char **)ppData) = pTable->p.data;
	return TRUE;
}
/*****************************************************************************
*
* dsGetRowCount - return table row count
*
* RETURNS: TRUE if success else FALSE
*/
int dsGetRowCount(unsigned *pRowCount, DS_DATASET_T *pTable)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	*pRowCount = pTable->elcount;
	return TRUE;
}
/*****************************************************************************
*
* dsMapTable - map dataset table to program variable
* 
* RETURNS: TRUE if success else FALSE
*/
int dsMapTable(DS_DATASET_T *pDataset, char *tableName,
	char *typeSpecifier, size_t *pCount, void *ppData)
{
	DS_DATASET_T *table;

	if (!dsFindTable(&table, pDataset, tableName, typeSpecifier)) {
		return FALSE;
	} 
	if (ppData == NULL || pCount == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR)
	}
	if (*(char **)ppData == NULL) {
		/*return location of existing table data */
		*(char **)ppData = table->p.data;
		*pCount = table->elcount;
		return TRUE;
	}
	/* set location for data to be read and return count to be read */
	if (table->p.data != NULL) {
		DS_ERROR(DS_E_TABLE_ALREADY_MAPPED);
	}
	if (table->maxcount > *pCount) {
		DS_ERROR(DS_E_MAP_AREA_TOO_SMALL);
	}
	table->p.data = *(char **)ppData;
	if(table->p.data == NULL && table->maxcount > 0) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	*pCount = table->maxcount;
	return TRUE;
}
/*****************************************************************************
*
* dsNewDataset - create an empty dataset with up to setDim -1 entries
* 
* RETURNS: TRUE if success else FALSE
*/
int dsNewDataset(DS_DATASET_T **ppDataset, char *name, size_t setDim)
{
	char tmpName[DS_NAME_DIM];
	size_t flags = 0;
	DS_DATASET_T *pDataset;

	if (ppDataset == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	if (setDim < 2) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (!dsCopyName(tmpName, name, NULL)) {
		DS_ERROR(DS_E_INVALID_DATASET_NAME);	
	}
	if ((pDataset = *ppDataset) == NULL) {
		if ((pDataset = (DS_DATASET_T *)
			dsDsetAlloc(setDim*sizeof(DS_DATASET_T))) == NULL) {
			return FALSE;
		}
		flags |= DS_ALLOC_NODE;
		*ppDataset = pDataset;
	}
	memset((char *)pDataset, 0, sizeof(DS_DATASET_T));
	strcpy(pDataset->name, name);
	pDataset->flags = flags;
	pDataset->maxcount = setDim - 1;
	pDataset->p.child = &pDataset[1];
	return TRUE;
}
/*****************************************************************************
*
* dsNewTable - create a table descriptor
*
* RETURNS: TRUE if success else FALSE
*/
int dsNewTable(DS_DATASET_T **ppTable, char *tableName,
	char *typeSpecifier, unsigned rowCount, void *pData)
{
	char name[DS_NAME_DIM];
	DS_DATASET_T *pTable;
	unsigned flags = 0, tid;

	if (ppTable == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	if (!dsCopyName(name, tableName, NULL)) {
		DS_ERROR(DS_E_INVALID_TABLE_NAME);
	}		
	if (!dsTypeId(&tid, typeSpecifier, NULL)) {
		return FALSE;
	}
	if ((pTable = *ppTable) == NULL) {
		if ((pTable = 
			(DS_DATASET_T *)dsDsetAlloc(sizeof(DS_DATASET_T))) == NULL) {
			return FALSE;
		}
		flags |= DS_ALLOC_NODE;
		*ppTable = pTable;
	}
	memset((char *)pTable, 0 , sizeof(DS_DATASET_T));
	strcpy(pTable->name, name);
	pTable->flags = flags;
	pTable->tid = tid;
	pTable->maxcount = rowCount;
	if ((pTable->p.data = pData) != NULL) {
		pTable->elcount = rowCount;
	}
	return TRUE;
}
