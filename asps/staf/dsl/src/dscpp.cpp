// Copyright 1998, Lawrence Berkeley Laboratory 

// dscpp.c - C++ wrappers

/*
modification history
--------------------
12jun98,whg	written.
*/

/*
DESCRIPTION
C++ classes to wrap dsl C functions
*/
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "dscpp.h"

#define DS_ABORT(m) {cerr << m << " " << __FILE__ << "." << __LINE__ <<\
						"\n" << flush; exit(0);}
//////////////////////////////////////////////////////////////////////////////
static int dsetCreate = 0, dsetDelete = 0, tblCreate = 0, tblDelete = 0;

void printCount(void)
{
	cout << "\ndsetCreate " << dsetCreate << ", dsetDelete " << dsetDelete;
	cout << " tblCreate " << tblCreate << ", tblDelete " << tblDelete << "\n";
}
//////////////////////////////////////////////////////////////////////////////
//
// wrappers for dsDataset
//
//----------------------------------------------------------------------------
dsDataset::dsDataset(DS_DATASET_T *pDataset)
{
	if (!DS_IS_DATASET(pDataset) || pDataset->cppWrap) {
		throw dsDatasetConstructFailure();
	}
	pSet = pDataset;
	pSet->cppWrap = (void *)this;
	dsetCreate++;
}
//----------------------------------------------------------------------------
dsDataset::dsDataset(char *name)
{
	if (!dsNewDataset(&pSet, name)) {
		throw dsDatasetConstructFailure();
	}
	pSet->cppWrap = (void *)this;
	dsetCreate++;
}
//----------------------------------------------------------------------------
dsDataset::~dsDataset(void)
{
	size_t i;
	dsDataset *pD;
	dsTable *pT;
	DS_DATASET_T *p;
	DS_LIST_T list;

	if (pSet != NULL) {
		if (!dsMakeFreeList(&list, pSet)) {
			DS_ABORT("~dsDataset() dsMakeFreeList() failed");
		}
		for (i = 0; i < list.count; i++) {
			p = list.pItem[i];
			if (p->cppWrap == NULL) {
				continue;
			}
			if (DS_IS_DATASET(p)) {
				pD = (dsDataset *)p->cppWrap;
				pD->pSet = NULL;
				if (pD != this) {
					delete pD;
				}
			}
			else if (DS_IS_TABLE(p)) {
				pT = (dsTable *)p->cppWrap;
				pT->pTable = NULL;
				delete pT;
			}
			else {
				DS_ABORT("~dsDataset() system error");
			}
		}
		if (!dsFreeListed(&list)) {
			DS_ABORT("~dsDelete() dsFreeListed failed");
		}
	}
	dsetDelete++;
}
//----------------------------------------------------------------------------
bool dsDataset::allocTables()
{
	return dsAllocTables(pSet) ? true : false;
}
//----------------------------------------------------------------------------
dsDataset * dsDataset::create(char *name)
{
	try {
		return new dsDataset(name);
	}
	catch (dsDataset::dsDatasetConstructFailure) {
		return NULL;
	}
}
//----------------------------------------------------------------------------
dsDataset *dsDataset::decode(XDR *xdrs)
{
	DS_DATASET_T *d = NULL;

	if (xdrs->x_op != XDR_DECODE) {
		DS_LOG_ERROR(DS_E_INVALID_XDR_OP);
		return NULL;
	}
	if (!xdr_dataset(xdrs, &d)) {
		return NULL;
	}
	if (DS_IS_TABLE(d)) {
		DS_LOG_ERROR(DS_E_INVALID_DATASET);
		dsFreeDataset(d);
		return NULL;
	}
	return new dsDataset(d);
}
//----------------------------------------------------------------------------
bool dsDataset::decodeData(XDR *xdrs)
{
	if (xdrs->x_op != XDR_DECODE) {
		DS_LOG_ERROR(DS_E_INVALID_XDR_OP);
		return false;
	}
	return xdr_dataset_data(xdrs, pSet) ? true : false;
}
//----------------------------------------------------------------------------
dsDataset *dsDataset::decodeType(XDR *xdrs)
{
	DS_DATASET_T *d = NULL;

	if (xdrs->x_op != XDR_DECODE) {
		DS_LOG_ERROR(DS_E_INVALID_XDR_OP);
		return NULL;
	}
	if (!xdr_dataset_type(xdrs, &d)) {
		return NULL;
	}
	if (DS_IS_TABLE(d)) {
		DS_LOG_ERROR(DS_E_INVALID_DATASET);
		dsFreeDataset(d);
		return NULL;
	}
	return new dsDataset(d);
}
//----------------------------------------------------------------------------
bool dsDataset::encodeBigEndian(XDR *xdrs)
{
	if (xdrs->x_op != XDR_ENCODE) {
		DS_LOG_ERROR(DS_E_INVALID_XDR_OP);
		return false;
	}
	return dsEncodeBigEndian(xdrs, pSet) ? true : false;
}
//----------------------------------------------------------------------------
bool dsDataset::encodeLittleEndian(XDR *xdrs)
{
	if (xdrs->x_op != XDR_ENCODE) {
		DS_LOG_ERROR(DS_E_INVALID_XDR_OP);
		return false;
	}
	return dsEncodeLittleEndian(xdrs, pSet) ? true : false;
}
//----------------------------------------------------------------------------
size_t dsDataset::entryCount()
{
	return pSet->elcount;
}
//----------------------------------------------------------------------------
bool dsDataset::entryIsDataset(size_t index)
{
	return index < pSet->elcount ?
		(bool) DS_IS_DATASET(pSet->p.link[index]) : false;
}
//----------------------------------------------------------------------------
bool dsDataset::entryIsTable(size_t index)
{
	return index < pSet->elcount ?
		(bool)DS_IS_TABLE(pSet->p.link[index]) : false;
}
//----------------------------------------------------------------------------
dsDataset *dsDataset::findDataset(char *name)
{
	DS_DATASET_T *pEntry;

	if (!dsFindEntry(&pEntry, pSet, name) || !DS_IS_DATASET(pEntry)) {
		return NULL;
	}
	return wrap(pEntry);
}
//----------------------------------------------------------------------------
dsTable *dsDataset::findTable(char *name)
{
	DS_DATASET_T *pEntry;

	if (!dsFindEntry(&pEntry, pSet, name) || !DS_IS_TABLE(pEntry)) {
		return NULL;
	}
	return dsTable::wrap(pEntry);
}
//----------------------------------------------------------------------------
dsDataset *dsDataset::getDataset(size_t index)
{
	return entryIsDataset(index) ?
		wrap(pSet->p.link[index]) : NULL;
}
//----------------------------------------------------------------------------

dsTable *dsDataset::getTable(size_t index)
{
	return entryIsTable(index) ?
		dsTable::wrap(pSet->p.link[index]) : NULL;
}
//----------------------------------------------------------------------------
bool dsDataset::isAcyclic()
{
	return dsIsAcyclic(pSet) ? true : false;
}
//----------------------------------------------------------------------------
bool dsDataset::link(dsDataset *pDataset)
{
	return dsLink(pSet, pDataset->pSet) ? true : false;
}
//----------------------------------------------------------------------------
bool dsDataset::link(dsTable *pTable)
{
	return dsLink(pSet, dsTable::cPtr(pTable)) ? true : false;
}
//----------------------------------------------------------------------------
bool dsDataset::linkAcyclic(dsDataset *pDataset)
{
	return dsLinkAcyclic(pSet, pDataset->pSet) ? true : false;
}
//----------------------------------------------------------------------------
size_t dsDataset::maxEntryCount()
{
	return pSet->maxcount;
}
//----------------------------------------------------------------------------
char *dsDataset::name()
{
	return pSet->name;
}
//----------------------------------------------------------------------------
int dsDataset::refcount()
{
	size_t n;

	return dsRefcount(&n, pSet) ? n : -1;
}
//----------------------------------------------------------------------------
bool dsDataset::unlink(dsDataset *pDataset)
{
	return dsUnlink(pSet, pDataset->pSet) ? true : false;
}
//----------------------------------------------------------------------------
bool dsDataset::unlink(dsTable *pTable)
{
	return dsUnlink(pSet, dsTable::cPtr(pTable)) ? true : false;
}
//----------------------------------------------------------------------------
dsDataset *dsDataset::wrap(DS_DATASET_T *d)
{
	if (!DS_IS_DATASET(d)) {
		return NULL;
	}
	return d->cppWrap ? (dsDataset *)d->cppWrap : new dsDataset(d);
}
//////////////////////////////////////////////////////////////////////////////
//
// wrappers for dsField
//
//----------------------------------------------------------------------------
dsField::dsField(DS_FIELD_T *pMyField)
{
	pField = pMyField;
	//CHECK FOR NULL
	pField->fieldWrap = (void *)this;
}
//----------------------------------------------------------------------------
size_t dsField::dim(size_t index)
{
	return index < dimCount() ? pField->dim[index] : 0;
}
//----------------------------------------------------------------------------
size_t dsField::dimCount(void)
{
	size_t count;
	for (count = 0; count <= DS_MAX_DIMS; count++) {
		if(pField->dim[count] == 0) {
			break;
		}
	}
	return count;
}
//----------------------------------------------------------------------------
size_t dsField::elcount()
{
	return pField->count;
}
//----------------------------------------------------------------------------
char *dsField::name()
{
	return pField->name;
}
//----------------------------------------------------------------------------
size_t dsField::offset()
{
	return pField->offset;
}
//----------------------------------------------------------------------------
size_t dsField::size()
{
	return pField->count*pField->type->size;
}
//----------------------------------------------------------------------------
size_t dsField::stdoffset()
{
	return pField->stdoffset;
}
//----------------------------------------------------------------------------
size_t dsField::stdsize()
{
	return pField->count*pField->type->stdsize;
}
//----------------------------------------------------------------------------
dsType *dsField::type()
{
	if (pField->type->typeWrap == NULL) {
		new dsType(pField->type);
	}
	return (dsType *)pField->type->typeWrap;
}
//////////////////////////////////////////////////////////////////////////////
//
// wrappers for dsTable
//
//----------------------------------------------------------------------------
dsTable::dsTable(DS_DATASET_T *pDataset)
{
	if (!DS_IS_TABLE(pDataset) || pDataset->cppWrap) {
		throw dsTableConstructFailure();
	}
	pTable = pDataset;
	pTable->cppWrap = (void *)this;
	tblCreate++;
}
//----------------------------------------------------------------------------
dsTable::dsTable(char *name, char *spec)
{
	if (!dsNewTable(&pTable, name, spec, 0, NULL)) {
		throw dsTableConstructFailure();
	}
	pTable->cppWrap = (void *)this;
	tblCreate++;
}
//----------------------------------------------------------------------------
//int dsNewTable(DS_DATASET_T **ppTable, char *tableName,
//	char *typeSpecifier, unsigned rowCount, void *pData)
dsTable::dsTable(char *name, char *spec, size_t rowCount, void *pData)
{
	if (!dsNewTable(&pTable, name, spec, rowCount, pData)) {
		throw dsTableConstructFailure();
	}
	pTable->cppWrap = (void *)this;
	tblCreate++;
}

//----------------------------------------------------------------------------
dsTable::~dsTable(void)
{
	if (pTable != NULL) {
		if (!dsFreeDataset(pTable)) {
			DS_ABORT("~dsTable() dsFreeDataset failed");
		}
	}
	tblDelete++;
}
//----------------------------------------------------------------------------
bool dsTable::allocTable()
{
	return dsAllocTables(pTable) ? true : false;
}
//----------------------------------------------------------------------------
void *dsTable::cellAddress(size_t rowNumber , size_t colNumber)
{
	char *p;
cout << "row " << rowNumber << ", col " << colNumber << ", elcount " << pTable->elcount << endl;
	return dsCellAddress(&p, pTable, rowNumber, colNumber) ?
		(void *)p : NULL;
}
//----------------------------------------------------------------------------
dsTable * dsTable::create(char *name, char *spec)
{
	try {
		return new dsTable(name, spec);
	}
	catch (dsTable::dsTableConstructFailure) {
		return NULL;
	}
}
//----------------------------------------------------------------------------
char *dsTable::dataAddress()
{
	return (char *)pTable->p.data;
}
//----------------------------------------------------------------------------
//int dsEquijoin(DS_DATASET_T *pJoinTable,DS_DATASET_T *pTableOne,
//	DS_DATASET_T *pTableTwo, char *aliases, char *joinLst, char *projectList);
bool dsTable::equijoin(dsTable *tableOne, dsTable *tableTwo,
			  char *aliases, char *joinLst, char *projectList)
{
	return dsEquijoin(pTable, cPtr(tableOne), cPtr(tableTwo),
		aliases, joinLst, projectList) ? true : false;
}
//----------------------------------------------------------------------------
bool dsTable::getCell(char *address, size_t rowNumber , size_t colNumber)
{
	return dsGetCell(address, pTable, rowNumber, colNumber) ? true : false;
}
//----------------------------------------------------------------------------
size_t dsTable::maxRowCount()
{
	return pTable->maxcount;
}
//----------------------------------------------------------------------------
char *dsTable::name()
{
	return pTable->name;
}
//----------------------------------------------------------------------------
void dsTable::printData(FILE *stream)
{
	dsPrintTableData(stream, pTable);
}
//----------------------------------------------------------------------------
void dsTable::printType(FILE *stream)
{
	dsPrintTableType(stream, pTable);
}
//----------------------------------------------------------------------------
//int dsProjectTable(DS_DATASET_T *pDst, DS_DATASET_T *pSrc, char *projectList);
bool dsTable::project(dsTable *srcTable, char *projectList)
{
	return dsProjectTable(pTable, cPtr(srcTable), projectList) ? true : false;
}
//----------------------------------------------------------------------------
//int dsPutCell(char *address, DS_DATASET_T *pTable,
//	size_t rowNumber , size_t colNumber);
bool dsTable::putCell(void *address, size_t rowNumber , size_t colNumber)
{
	return dsPutCell((char *)address, pTable, rowNumber, colNumber) ? true : false;
}
//----------------------------------------------------------------------------
//int dsReallocTable(DS_DATASET_T *pTable, size_t nRow);
bool dsTable::realloc(size_t maxRowCount)
{
	return dsReallocTable(pTable, maxRowCount) ? true : false;
}
//----------------------------------------------------------------------------
//int dsTableRowCount(size_t *pRowCount, DS_DATASET_T *pTable);
size_t dsTable::rowCount()
{
	return pTable->elcount;
}
//----------------------------------------------------------------------------
dsType *dsTable::rowType(void)
{
	DS_TYPE_T *p;

	if (!dsTypePtr(&p, pTable->tid)) {
		DS_LOG_ERROR(DS_E_SYSTEM_ERROR);
		return NULL;
	}
	if(p->typeWrap == NULL) {
		new dsType(p);
	}
	return (dsType *)p->typeWrap;
}
//----------------------------------------------------------------------------
//int dsSetTableRowCount(DS_DATASET_T *pTable, size_t rowCount);
bool dsTable::setRowCount(size_t rowCount)
{
	return dsSetTableRowCount(pTable, rowCount) ? true : false;
}
//----------------------------------------------------------------------------
dsTable *dsTable::targetTable(char *tableName, char *typeName,
							  dsTable *srcTable, char *projectList)
{
	DS_DATASET_T *pTarget;
	return dsTargetTable(&pTarget, tableName, typeName, cPtr(srcTable),
		NULL, NULL, projectList) ? wrap(pTarget) : NULL;
}
//----------------------------------------------------------------------------
//int dsTargetTable(DS_DATASET_T **ppTable, char *tableName, char *typeName, 
//	DS_DATASET_T *parentOne, DS_DATASET_T *parentTwo, 
//	char *aliases, char *projectList);
dsTable *dsTable::targetTable(char *tableName, char *typeName,
							  dsTable *parentOne, dsTable *parentTwo,
							  char *aliases, char *projectList)
{
	DS_DATASET_T *pTarget;
	return dsTargetTable(&pTarget, tableName, typeName, cPtr(parentOne),
		cPtr(parentTwo), aliases, projectList) ? wrap(pTarget) : NULL;
}
//----------------------------------------------------------------------------
dsTable *dsTable::wrap(DS_DATASET_T *d)
{
	if (!DS_IS_TABLE(d)) {
		return NULL;
	}
	return d->cppWrap ? (dsTable *)d->cppWrap : new dsTable(d);
}
//////////////////////////////////////////////////////////////////////////////
//
// wrappers for dsType
//
//----------------------------------------------------------------------------
dsType::dsType(DS_TYPE_T *pMyType)
{
	pType = pMyType;
//CHECK TYPEWRAP FOR !NULL
	pType->typeWrap = (void *)this;
}
//----------------------------------------------------------------------------
int dsType::code(void)
{
	return pType->code;
}
//----------------------------------------------------------------------------
//int dsFindColumn(size_t *pColNumber, DS_DATASET_T *pTable, char *name);
dsField * dsType::field(char *name)
{
	size_t i;

	for (i = 0; i < pType->nField; i++) {
		if (strcmp(name, pType->field[i].name) == 0) {
			return field(i);
		}
	}
	return NULL;
}
//----------------------------------------------------------------------------
dsField * dsType::field(size_t index)
{
	if (index < pType->nField) {
		if (pType->field[index].fieldWrap == NULL) {
			new dsField(&pType->field[index]);
		}
		return (dsField *)pType->field[index].fieldWrap;
	}
	return NULL;
}
//----------------------------------------------------------------------------
size_t dsType::fieldCount(void)
{
	return pType->nField;
}
//----------------------------------------------------------------------------
bool dsType::isSpecifier(char *spec)
{
	size_t tid;

	if (!dsTypeId(&tid, spec, NULL)) {
		return false;
	}
	return tid == pType->tid ? true : false;
}
//----------------------------------------------------------------------------
char *dsType::name(void)
{
	return pType->name;
}
//----------------------------------------------------------------------------
size_t dsType::size(void)
{
	return pType->size;
}
//----------------------------------------------------------------------------
char *dsType::specifier(void)
{
	char *str;

	return dsTypeSpecifier(&str, pType->tid) ? str : NULL;
}
//----------------------------------------------------------------------------
size_t dsType::stdsize()
{
	return pType->stdsize;
}
