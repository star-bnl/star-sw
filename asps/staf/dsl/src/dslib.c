/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dslib.c - routines  datasets */

/*
modification history
--------------------
30jul93,whg  written.
11feb95,whg  removed pUser from functions
26apr95,whg  add new API functions
11jun96,whg  added indirection to dataset structure
*/
/*
DESCRIPTION
interface routines for definition of datasets and mapping
tables to program variables 
*/
#define DS_PRIVATE
#include <stdlib.h>
#include <string.h>
#include "dstype.h"

/*****************************************************************************
*
* dsCellAddress - return address of a table cell
*
* RETURNS: TRUE if success else FALSE
*/
int dsCellAddress(char **pAddress, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber)
{
	DS_FIELD_T *pField;
	DS_TYPE_T *pType;

	if (!dsTableType(&pType, pTable) ||
		!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	if (rowNumber >= pTable->elcount) {
		DS_ERROR(DS_E_INVALID_ROW_NUMBER);
	}
	*pAddress = (char *)pTable->p.data + rowNumber*pType->size + pField->offset;
	return TRUE;
}
/*****************************************************************************
*
* dsGetCell - copy data from a table cell to a program variable
*
* RETURNS: TRUE if success else FALSE
*/
int dsGetCell(char *address, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber)
{
	char *src;
	size_t size;

	if (!dsCellAddress(&src, pTable, rowNumber, colNumber) ||
		!dsColumnSize(&size, pTable, colNumber)) {
		return FALSE;
	}
	memcpy(address, src, size);
	return TRUE;
}
/*****************************************************************************
*
* dsPutCell - copy data from a program variable to a table cell
*
* RETURNS: TRUE if success else FALSE
*/
int dsPutCell(const char *address, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber)
{
	char *dst;
	size_t size;

	if (!dsCellAddress(&dst, pTable, rowNumber, colNumber) ||
		!dsColumnSize(&size, pTable, colNumber)) {
		return FALSE;
	}
	memcpy((char*)dst, address, size); 
	return TRUE;
}
/*****************************************************************************
*
* dsColumnDimCount - return the dimensionally of a column of array type 
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnDimCount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber)
{
	size_t count;
	DS_FIELD_T *pField;

	if (!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	for (count = 0; count <= DS_MAX_DIMS; count++) {
		if(pField->dim[count] == 0) {
			break;
		}
	}
	*pCount = count;
	return TRUE; 
}
/*****************************************************************************
*
* dsColumnDimensions - return the dimensions of a column of array type
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnDimensions(size_t *dims, DS_DATASET_T *pTable, size_t colNumber)
{
	size_t i;
 	DS_FIELD_T *pField;

	if (!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	for (i = 0; i <= DS_MAX_DIMS; i++) {
		if(pField->dim[i] == 0) {
			break;
		}
		dims[i] = pField->dim[i];
	}
	return TRUE; 


}
/*****************************************************************************
*
* dsColumnElcount - return the number of elements in a column
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnElcount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber)
{
  	DS_FIELD_T *pField;

	if (!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	*pCount = pField->count;
	return TRUE;
}
/*****************************************************************************
*
* dsColumnField - return pointer to field definition for a column
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnField(DS_FIELD_T **ppField, DS_DATASET_T *pTable, size_t colNumber)
{
	DS_FIELD_T *pField;
	DS_TYPE_T *pType;

	if (!dsTableType(&pType, pTable)) {
		return FALSE;
	}
	if (colNumber >= pType->nField) {
		DS_ERROR(DS_E_INVALID_COLUMN_NUMBER);
	}
	pField =DS_FIELD_PTR(pType);
	*ppField = &pField[colNumber];
	return TRUE; 
}
/*****************************************************************************
*
* dsColumnName - return the name of a column in a table
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnName(const char **pName, DS_DATASET_T *pTable, size_t colNumber)
{
	DS_FIELD_T *pField;

	if (!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	*pName = pField->name;
	return TRUE;
}
/*****************************************************************************
*
* dsColumnSize - return the size of a column in bytes
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnSize(size_t *pSize, DS_DATASET_T *pTable, size_t colNumber)
{
	DS_FIELD_T *pField;

	if (!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	*pSize = pField->count*pField->type->size;
	return TRUE;
}
/*****************************************************************************
*
* dsColumnTypeCode - return the type code for elements of a column
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnTypeCode(DS_TYPE_CODE_T *pCode, DS_DATASET_T *pTable, size_t colNumber)
{
	DS_FIELD_T *pField;

	if (!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	*pCode = pField->type->code;
	return TRUE;
}
/*****************************************************************************
*
* dsColumnTypeName - return the type name for elements of a column
*
* RETURNS: TRUE if success else FALSE
*/
int dsColumnTypeName(const char **pName, DS_DATASET_T *pTable, size_t colNumber)
{
	DS_FIELD_T *pField;

	if (!dsColumnField(&pField, pTable, colNumber)) {
		return FALSE;
	}
	*pName = pField->type->name;
	return TRUE;
}
/*****************************************************************************
*
* dsDatasetEntry - return a handle for a dataset entry
*
* RETURNS: TRUE if success else FALSE
*/
int dsDatasetEntry(DS_DATASET_T **ppEntry,
	DS_DATASET_T *pDataset, size_t entryNumber)
{
	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	if (pDataset->elcount < entryNumber) {
		DS_ERROR(DS_E_INVALID_ENTRY_NUMBER);
	}
	*ppEntry = pDataset->p.link[entryNumber];
	return TRUE;
}
/*****************************************************************************
*
* dsDatasetEntryCount - return the number of entries in a dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsDatasetEntryCount(size_t *pCount, DS_DATASET_T *pDataset)
{
	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	*pCount = pDataset->elcount;
	return TRUE;
}
/*****************************************************************************
*
* dsDatasetMaxEntryCount - return the maximum number of entries for a dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsDatasetMaxEntryCount(size_t *pCount, DS_DATASET_T *pDataset)
{
	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	*pCount = pDataset->maxcount;
	return TRUE;
}
/*****************************************************************************
*
* dsDatasetName - return a pointer to a dataset name
*
* RETURNS: TRUE if success else FALSE
*/
int dsDatasetName(const char **pName, DS_DATASET_T *pDataset)
{
	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	*pName = pDataset->name;
	return TRUE;
}
/*****************************************************************************
*
* dsFindColumn - return the column number for a named column
*
* RETURNS: TRUE if success else FALSE
*/
int dsFindColumn(size_t *pColNumber, DS_DATASET_T *pTable, const char *name)
{
	DS_FIELD_T *pField;
	DS_TYPE_T *pType;

	if (!dsTableType(&pType, pTable)) {
		return FALSE;
	}
	if (dsFindField(&pField, pType, name) != 0) {
		return FALSE;
	}
	*pColNumber = pField - DS_FIELD_PTR(pType);
	return TRUE;
}

/*****************************************************************************
*
* dsFindEntry - search for a dataset entry by path name
*
* RETURNS: TRUE if success else FALSE
*/
int dsFindEntry(DS_DATASET_T **ppEntry, DS_DATASET_T *pDataset, const char *path)
{
	int c;
	size_t i;

	if (!DS_IS_DATASET(pDataset)) {
		DS_ERROR(DS_E_DATASET_REQUIRED);
	}
	for (i = 0; i < pDataset->elcount; i++) {
		if ((c = dsCmpName(pDataset->p.link[i]->name, path)) <= 0) {
			if (c < 0) {
				DS_ERROR(DS_E_NAMES_COLLIDE);
			}
			*ppEntry = pDataset->p.link[i];
			return TRUE;
		}
	}
	DS_ERROR(DS_E_ENTRY_NOT_FOUND);
}
/*****************************************************************************
*
* dsFindTable - find table in dataset and return pointer to descriptor
*
* RETURNS: TRUE if success else FALSE
*/
int dsFindTable(DS_DATASET_T **ppTable,
	DS_DATASET_T *pDataset, const char *name, const char *typeSpecifier)
{
	int result;
	DS_DATASET_T *table;

	if (!dsFindEntry(&table, pDataset, name) ||
		!dsTableIsType(&result, table, typeSpecifier)) {
		return FALSE;
	}
	if (!result) {
		DS_ERROR(DS_E_TYPE_MISSMATCH);
	}
	*ppTable = table;
	return TRUE;
}
/*****************************************************************************
*
* dsIsDataset - checks for dataset handle
*
* RETURNS: TRUE if success else FALSE
*/
int dsIsDataset(bool_t *pResult, DS_DATASET_T *handle)
{
	if (!DS_IS_VALID(handle)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	*pResult = DS_IS_DATASET(handle);
	return TRUE;
}
/*****************************************************************************
*
* dsIsTable - checks for table handle
*
* RETURNS: TRUE if success else FALSE
*/
int dsIsTable(bool_t *pResult, DS_DATASET_T *handle)
{
	if (!DS_IS_VALID(handle)) {
		DS_ERROR(DS_E_INVALID_DATASET);
	}
	*pResult = DS_IS_TABLE(handle);
	return TRUE;
}
/*****************************************************************************
*
* dsMapTable - map dataset table to program variable
* 
* RETURNS: TRUE if success else FALSE
*/
int dsMapTable(DS_DATASET_T *pDataset, const char *tableName,
	const char *typeSpecifier, size_t *pCount, char **ppData)
{
	DS_DATASET_T *table;

	if (!dsFindTable(&table, pDataset, tableName, typeSpecifier)) {
		return FALSE;
	} 
	if (ppData == NULL || pCount == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR)
	}
	if (*ppData == NULL) {
		/*return location of existing table data */
		*ppData = table->p.data;
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
	table->p.data = *ppData;
	if(table->p.data == NULL && table->maxcount > 0) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	*pCount = table->maxcount;
	return TRUE;
}
/*****************************************************************************
*
* dsRefcount - reference count for dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsRefcount(size_t *pCount, DS_DATASET_T *pDataset)
{
	if (pDataset == NULL || pCount == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	*pCount = pDataset->refcount;
	return TRUE;
}
/*****************************************************************************
*
* dsSetTableRowCount - set the row count for a table
*
* RETURNS: TRUE if success else FALSE
*/
int dsSetTableRowCount(DS_DATASET_T *pTable, size_t rowCount)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
 
 	/* OK to set a table to it's current row count */
 	if (pTable->elcount == rowCount) return TRUE;
 
	if (rowCount > pTable->maxcount || pTable->p.data == NULL) {
		DS_ERROR(DS_E_INVALID_ROW_COUNT);
	}
	pTable->elcount = rowCount;
	return TRUE;
}
/*****************************************************************************
*
* dsTableColumnCount - return the number of columns in a table
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableColumnCount(size_t *pCount, DS_DATASET_T *pTable)
{
	DS_TYPE_T *pType;

	if (!dsTableType(&pType, pTable)) {
		return FALSE;
	}
	*pCount = pType->nField;
	return TRUE;
}
/*****************************************************************************
*
* dsTableDataAddress - return address of a table's data
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableDataAddress(char **pAddress, DS_DATASET_T *pTable)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	*pAddress = (char *)pTable->p.data;
	return TRUE;
}
/*****************************************************************************
*
* dsTableIsType - compares a table row type with a C++ type specifier
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableIsType(bool_t *pResult, DS_DATASET_T *pTable, const char *specifier)
{
	size_t tid;

	if (!dsTypeId(&tid, specifier, NULL)) {
		return FALSE;
	}
	*pResult = tid == pTable->tid;
	return TRUE;
}
/*****************************************************************************
*
* dsTableMaxRowCount - return the maximum number of rows for a table
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableMaxRowCount(size_t *pCount, DS_DATASET_T *pTable)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	*pCount = pTable->maxcount;
	return TRUE;
}
/*****************************************************************************
*
* dsTableName - return the name of a table
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableName(const char **pName, DS_DATASET_T *pTable)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	*pName = pTable->name;
	return TRUE;
}
/*****************************************************************************
*
* dsTableRowCount - return table row count
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableRowCount(size_t *pRowCount, DS_DATASET_T *pTable)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	*pRowCount = pTable->elcount;
	return TRUE;
}
/*****************************************************************************
*
* dsTableRowSize - return the size of a row in bytes
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableRowSize(size_t *pSize, DS_DATASET_T *pTable)
{
	DS_TYPE_T *pType;

	if (!dsTableType(&pType, pTable)) {
		return FALSE;
	}
	*pSize = pType->size;
	return TRUE;
}
/*****************************************************************************
*
* dsTableType - get table type
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableType(DS_TYPE_T **ppType, DS_DATASET_T *pTable)
{
	if (!DS_IS_TABLE(pTable)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	return dsTypePtr(ppType, pTable->tid);
}
/*****************************************************************************
*
* dsTableTypeName - return the name for the type of a row
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableTypeName(const char **pName, DS_DATASET_T *pTable)
{
	DS_TYPE_T *pType;

	if (!dsTableType(&pType, pTable)) {
		return FALSE;
	}
	*pName = pType->name;
	return TRUE;
}
/*****************************************************************************
*
* dsTableTypeSpecifier - return a C++ style type specifier for the type of a row
*
* RETURNS: TRUE if success else FALSE
*/
int dsTableTypeSpecifier(const char **pSpecifier, DS_DATASET_T *pTable)
{
	return dsTypeSpecifier(pSpecifier, NULL, pTable->tid);
}
