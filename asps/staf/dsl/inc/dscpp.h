/* Copyright 1998, Lawrence Berkeley Laboratory */

/* dscpp.h - include file for C++ wrappers */

/*
modification history
--------------------
12jun98,whg  written.
*/				
/*
DESCRIPTION
definitions for dsl C++ classes
*/
#define DS_PRIVATE
#include "dsxdr.h"

#ifdef sun
typedef int bool;
#define true 1
#define false 0
#endif /*sun */

class dsTable;
class dsType;
// To do
//int dsMapTable(DS_DATASET_T *pDataset, char *name,
//	char *typeSpecifier, size_t *pCount, char **ppData);

// Following not wrapped
//bool_t xdr_dataset_skip(XDR *xdrs);

//int dsAddTable(DS_DATASET_T *pDataset, char *name,
//	char *typeSpecifier, size_t nRow, char **ppData);
//**********use new dsTable() and dsDataset::link()

//int dsInitTable(DS_DATASET_T *pTable, char *tableName,
//	char *typeSpecifier, unsigned rowCount, void *pData);
//**********use new dsTable()

//int dsTasProject(DS_DATASET_T *pDataset, char *name,
//	char *typeSpecifier, size_t *pCount, void *ppData);

//////////////////////////////////////////////////////////////////////////////
//
//
//
class dsDataset {
private:
	DS_DATASET_T *pSet;
	dsDataset(DS_DATASET_T *pDataset);
	dsDataset(const dsDataset& d);
	dsDataset& operator=(const dsDataset& d);
	static dsDataset *wrap(DS_DATASET_T *d);
public:
	// only public constructor
	dsDataset(char *name);

	// exception class for failure of "new dsDataset(name)"
	class dsDatasetConstructFailure {};

	//int dsFreeDataset(DS_DATASET_T *pDataset);
	~dsDataset();

	//int dsAllocTables(DS_DATASET_T *pDataset);
	bool allocTables(void);

	//int dsNewDataset(DS_DATASET_T **ppDataset, char *name);
	static dsDataset *create(char *name);

	//bool_t xdr_dataset(XDR *xdrs, DS_DATASET_T **ppDataset);
	static dsDataset *decode(XDR *xdrs);

	//bool_t xdr_dataset_data(XDR *xdrs, DS_DATASET_T *pDataset);
	bool decodeData(XDR *xdrs);

	//bool_t xdr_dataset_type(XDR *xdrs, DS_DATASET_T **ppDataset);
	static dsDataset *decodeType(XDR *xdrs);

	//int dsEncodeLittleEndian(XDR *xdrs, DS_DATASET_T *pDataset);
	bool encodeBigEndian(XDR *xdrs);

	//int dsEncodeLittleEndian(XDR *xdrs, DS_DATASET_T *pDataset);
	bool encodeLittleEndian(XDR *xdrs);

	//int dsDatasetEntryCount(size_t *pCount, DS_DATASET_T *pDataset);
	size_t entryCount(void);

	//int dsIsDataset(int *pResult, DS_DATASET_T *handle);
	bool entryIsDataset(size_t index);

	//int dsIsTable(int *pResult, DS_DATASET_T *handle);
	bool entryIsTable(size_t index);

	//int dsFindEntry(DS_DATASET_T **ppEntry, DS_DATASET_T *pDataset, char *path);
	dsDataset *findDataset(char *name);
	dsTable *findTable(char *name);

	//int dsDatasetEntry(DS_DATASET_T **ppEntry, DS_DATASET_T *pDataset,
	//	size_t entryNumber); 
	dsDataset *getDataset(size_t index);
	dsTable *getTable(size_t index);

	//int dsIsAcyclic(DS_DATASET_T *dataset);
	bool isAcyclic(void);

	//int dsLink(DS_DATASET_T *pParent, DS_DATASET_T *pChild);
	bool link(dsDataset *pSet);
	bool link(dsTable *pTable);

	//int dsLinkAcyclic(DS_DATASET_T *pParent, DS_DATASET_T *pChild);
	bool linkAcyclic(dsDataset *pSet);

	//int dsDatasetMaxEntryCount(size_t *pCount, DS_DATASET_T *pDataset);
	size_t maxEntryCount(void);  //NEEDED??

	//int dsDatasetName(char **pName, DS_DATASET_T *pDataset);
	char *name(void);

	//int dsRefcount(size_t *pCount, DS_DATASET_T *pDataset);
	int refcount(void);

	// int dsUnlink(DS_DATASET_T *pParent, DS_DATASET_T *pChild);
	bool unlink(dsDataset *pSet);
	bool unlink(dsTable *pTable);

};
//////////////////////////////////////////////////////////////////////////////
//
//
//
class dsField {
private:
	DS_FIELD_T *pField;
	dsField(DS_FIELD_T *pField);
	dsField(const dsField& f);
	dsField& operator=(const dsField& f);
	~dsField();
	friend class dsType;
public:
	//int dsColumnDimensions(size_t *dims, DS_DATASET_T *pTable, size_t colNumber);
	size_t dim(size_t index);

	//int dsColumnDimCount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber);
	size_t dimCount(void);

	//int dsColumnElcount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber);
	size_t elcount(void);

	//int dsColumnName(char **pName, DS_DATASET_T *pTable, size_t colNumber);
	char *name(void);

	size_t offset(void);

	//int dsColumnSize(size_t *pSize, DS_DATASET_T *pTable, size_t colNumber);
	size_t size(void);

	size_t stdoffset(void);

	size_t stdsize(void);

	dsType *type(void);
};
//////////////////////////////////////////////////////////////////////////////
//
//
//
class dsTable {
private:
	DS_DATASET_T *pTable;
	static DS_DATASET_T *cPtr(dsTable *cppPtr) {return cppPtr ? cppPtr->pTable : NULL;}
	dsTable(DS_DATASET_T *pTable);
	dsTable(const dsTable& t);
	dsTable& operator=(const dsTable& t);
	static dsTable *wrap(DS_DATASET_T *p);
	friend class dsDataset;
public:
	//int dsNewTable(DS_DATASET_T **ppTable, char *tableName,
	//	char *typeSpecifier, unsigned rowCount, void *pData)
	dsTable(char *name, char *spec);
	dsTable(char *name, char *spec, size_t rowCount, void *pData);

	// exception class for failure of "new dsTable(name, spec)"
	class dsTableConstructFailure {};

	// dsFreeDataset(
	~dsTable(void);

	//int dsAllocTables(DS_DATASET_T *pDataset);
	bool allocTable();

	//int dsCellAddress(char **pAddress, DS_DATASET_T *pTable,
	//	size_t rowNumber , size_t colNumber);
	void *cellAddress(size_t rowNumber , size_t colNumber);

	//int dsNewTable(DS_DATASET_T **ppTable, char *tableName,
	//	char *typeSpecifier,  unsigned rowCount, void *pData);
	static dsTable * create(char *name, char *spec);

	//int dsTableDataAddress(char **pAddress, DS_DATASET_T *pTable);
	char *dataAddress(void);

	//int dsEquijoin(DS_DATASET_T *pJoinTable,DS_DATASET_T *pTableOne,
	//	DS_DATASET_T *pTableTwo, char *aliases, char *joinLst, char *projectList);
	bool equijoin(dsTable *tableOne, dsTable *tableTwo,
		char *aliases, char *joinLst, char *projectList);

	//int dsGetCell(char *address, DS_DATASET_T *pTable,
	//	size_t rowNumber , size_t colNumber); 
	bool getCell(char *address, size_t rowNumber , size_t colNumber);

	//int dsTableMaxRowCount(size_t *pCount, DS_DATASET_T *pTable);
	size_t maxRowCount(void);

	//int dsTableName(char **pName, DS_DATASET_T *pTable);
	char *name(void);

	//void dsPrintTableData(FILE *stream, DS_DATASET_T *table);
	void printData(FILE *stream);

	//void dsPrintTableType(FILE *stream, DS_DATASET_T *pTable);
	void printType(FILE *stream);

	//int dsProjectTable(DS_DATASET_T *pDst, DS_DATASET_T *pSrc, char *projectList);
	bool project(dsTable *srcTable, char *projectList);

	//int dsPutCell(char *address, DS_DATASET_T *pTable,
	//	size_t rowNumber , size_t colNumber);
	bool putCell(void *address, size_t rowNumber , size_t colNumber);

	//int dsReallocTable(DS_DATASET_T *pTable, size_t nRow);
	bool realloc(size_t maxRowCount);

	//int dsTableRowCount(size_t *pRowCount, DS_DATASET_T *pTable);
	size_t rowCount(void);

	dsType *rowType(void);

	//int dsSetTableRowCount(DS_DATASET_T *pTable, size_t rowCount);
	bool setRowCount(size_t rowCount);

	//int dsTargetTable(DS_DATASET_T **ppTable, char *tableName, char *typeName, 
	//	DS_DATASET_T *parentOne, DS_DATASET_T *parentTwo, 
	//	char *aliases, char *projectList);
	static dsTable *targetTable(char *tableName, char *typeName, dsTable *parentOne,
		dsTable *parentTwo, char *aliases, char *projectList);
	static dsTable *targetTable(char *tableName, char *typeName,
		dsTable *srcTable, char *projectList);

};
//////////////////////////////////////////////////////////////////////////////
//
//
//
class dsType {
private:
	DS_TYPE_T *pType;
	dsType(DS_TYPE_T *pMyType);
	dsType(const dsType& t);
	dsType& operator=(const dsType& t);
	~dsType();
	friend class dsTable;
	friend class dsField;

public:
	//int dsColumnTypeCode(DS_TYPE_CODE_T *pCode, DS_DATASET_T *pTable, size_t colNumber);
	int code(void);

	//int dsFindColumn(size_t *pColNumber, DS_DATASET_T *pTable, char *name);
	dsField * field(char *name);
	dsField * field(size_t index);
	
	//int dsTableColumnCount(size_t *pCount, DS_DATASET_T *pTable);
	size_t fieldCount(void);

	//int dsTableIsType(int *pResult, DS_DATASET_T *pTable, char *specifier);
	bool isSpecifier(char *spec);

	//int dsColumnTypeName(char **pName, DS_DATASET_T *pTable, size_t colNumber);
	//int dsTableTypeName(char **pName, DS_DATASET_T *pTable);
	char *name(void);

	//int dsTableRowSize(size_t *pSize, DS_DATASET_T *pTable);
	size_t size(void);

	//int dsTableTypeSpecifier(char **pSpecifier, DS_DATASET_T *pTable);
	char *specifier(void);

	size_t stdsize(void);
};
