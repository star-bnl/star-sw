/* Copyright 1993, Lawrence Berkeley Laboratory */

/* testlib.c - test abstract type functions */

/*
modification history
--------------------
26jul93,whg  made from testadt.c testhash.c testtree.c testxdr.c.
25feb95,whg  modify for CORBA IDL type system
11jun96,whg  added indirection to dataset structure
*/

/*
DESCRIPTION
collection of routine to test ds lib
*/
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#define DS_PRIVATE
#include "asuAlloc.h"
#include "dsxdr.h"


#define NLOOP 100
#define XDR_MEM_SIZE(nloop) ((nloop)*(300 + 40*(nloop)))
#define DS_TEST_FAILED(msg) {dsErrorPrint("TEST FAILED: %s - %s.%d\n",\
	 msg, __FILE__, __LINE__); dsPerror(""); return FALSE;}

void dumpType(DS_TYPE_T *type);
static void dumpTypeR(DS_TYPE_T *type, char *prefix);
/******************************************************************************
*
* dumpType - print info about type
*
*/
void dumpType(DS_TYPE_T *type)
{
	printf(" offset  count std type\n\n");
	dumpTypeR(type, "");
}
/******************************************************************************
*
* dumpTypeR - recursive part of dumpType
*
*/
static void dumpTypeR(DS_TYPE_T *type, char *prefix)
{
	char s[100];
	size_t i, j;
	DS_FIELD_T *field;

	for (field = DS_FIELD_PTR(type), i = 0; i < type->nField; i++, field++) {
		printf("%7d%7d",field->offset,  field->count);
		printf("  %c  ", DS_REP_IS_STD(field->type) ? 'T' : 'F');
		printf("%10s |", field->type->name);
		printf("%s%s", prefix, field->name);
		for (j = 0; field->dim[j] && j < DS_MAX_DIMS; j++) {
			printf("[%d]", field->dim[j]);
		}
		printf("\n");
		if (field->type->code == DS_TYPE_STRUCT) {
			strcpy(s, "    ");
			strcat(s, prefix);
			strcat(s, field->name);
			strcat(s, ".");
			dumpTypeR(field->type, s);
			continue;
		}
	}
}
/******************************************************************************
*
* dsTestApi - simple test API
*
*/
int dsTestApi(void)
{
	char buf[10], *ptr, *name; const char *dsName = "dsName";
	char *pDataOne = NULL, *pDataTwo = NULL;
	const char *specOne = "struct typeOne {float x, y, z; long data;}";
	const char *specTwo = "struct typeTwo {char name[10]; long age; short flag;}";
	const char *specThree = "struct typeThree {octet b; double d; short a[2][3];}";
	typedef struct typeTwo {char name[10]; long age; short flag;}TYPE_TWO;
	TYPE_TWO *pRow;
	const char *tableNameOne = "tableNameOne", *tableNameTwo = "tableNameTwo";
	bool_t result;
	size_t count, rowCountOne = 10, rowCountTwo = 13;
	const char *specifier;
	size_t colNumber = 0, dims[5], size;
	DS_DATASET_T *pDataset, *pEntry, *pTable;
	DS_TYPE_CODE_T code;

	if (!dsNewDataset(&pDataset, dsName) ||
		!dsIsDataset(&result, pDataset) || !result ||
		!dsDatasetName((const char **)&name, pDataset) ||
		strcmp(name, dsName)) {
		DS_TEST_FAILED("dsNewDataset");
	}
	if (!dsAddTable(pDataset, tableNameOne, specOne, rowCountOne, &pDataOne) ||
		!dsAddTable(pDataset, tableNameTwo, specTwo, rowCountTwo, &pDataTwo) ||
		!dsDatasetEntryCount(&count, pDataset) || count != 2 ||
		!dsDatasetEntry(&pEntry, pDataset, 1) ||
		!dsFindEntry(&pTable, pDataset, tableNameTwo) || pEntry != pTable || 
		!dsIsTable(&result, pTable) || !result ||
		!dsTableIsType(&result, pTable, specTwo) || !result) {
		DS_TEST_FAILED("dsAddTable");
	}
	if (!dsTableColumnCount(&count, pTable) || count != 3 ||
		!dsTableDataAddress(&ptr, pTable) || ptr != pDataTwo ||
  		!dsTableMaxRowCount(&count, pTable) || count != rowCountTwo ||
		!dsSetTableRowCount(pTable, 9) ||
		!dsTableRowCount(&count, pTable) || count != 9 ||
		!dsTableName((const char**)&name, pTable) || strcmp(name, tableNameTwo) ||
		!dsTableRowSize(&size, pTable) || size != sizeof(TYPE_TWO) ||
		!dsTableTypeName((const char**)&name, pTable) || strcmp(name, "typeTwo") ||
		!dsTableTypeSpecifier(&specifier, pTable)) {
		DS_TEST_FAILED("table attributes failed");
	}
	pRow = (TYPE_TWO *)pDataTwo;
	printf("type specifier for tableTwo:\n\t%s\n", specifier);
  	if (!dsCellAddress(&name, pTable, 2, 0) || name != (pDataTwo + 2*size) ||
		!dsPutCell("test", pTable, 2, 0) ||
		!dsGetCell(buf, pTable, 2, 0) || strcmp(buf, "test") ||
		strcmp(pRow[2].name, "test")) {
		DS_TEST_FAILED("cell function failed");
	}

	if (!dsColumnDimCount(&count, pTable, colNumber) || count != 1 ||
		!dsColumnDimensions(dims, pTable, colNumber) || dims[0] != 10 ||
		!dsColumnElcount(&count, pTable, colNumber) || count != 10 ||
		!dsColumnName((const char**)&name, pTable, colNumber) || strcmp(name, "name") ||
		!dsColumnSize(&size, pTable, colNumber) || size != 10 ||
		!dsColumnTypeCode(&code, pTable, colNumber) || code != DS_TYPE_CHAR ||
		!dsColumnTypeName((const char**)&name, pTable, colNumber) || strcmp(name, "char") ||
		!dsFindColumn(&count, pTable, "flag") || count != 2) {
		DS_TEST_FAILED("column attributes failed");
 	}
	if (!dsNewDataset(&pEntry, "childDataset") ||
		!dsLink(pDataset, pEntry) ||
		!dsIsDataset(&result, pEntry) || !result ||
		!dsAddTable(pEntry, "tableThree", specThree, 7, NULL)) {
		DS_TEST_FAILED("dsLink failed");
	}
	dsPrintSpecifiers(stdout, pDataset);
	dsFreeDataset(pDataset);
	return TRUE;
}
/******************************************************************************
*
* dsTestCorba - simple test of abstract data type routines
*
*/
int dsTestCorba()
{
	const char *str1 = "struct test {struct s {long x, y;}z;\n"
		"long l; struct k{struct s{double u, v;}news;}kk;\n"
		"short sh[10][12]; s q; k v;}";
	char buf[200];const char *ptr;
	size_t i, n, tid;
	DS_TYPE_T *type = NULL;

	const char *str2 = str1;
	printf("strlen %d\n\n%s\n", strlen(str1), str1);
	
	if (!dsParseType(&type, &n, str1, NULL)) {
		dsPerror("dsParseType failed");
		return FALSE;
	}
	if (!dsFormatTypeSpecifier(buf, sizeof(buf), type)) {
		dsPerror("dsFormatTypeSpecifier failed");
		return FALSE;
	}
	printf("\ndsFormatTypeSpecifier: len %d\n%s\n", strlen(buf), buf);
	if (!dsTypeId(&tid, str1, &ptr)) {
		dsPerror("dsTypeId failed");
		return FALSE;
	}
	if (!dsTypeSpecifier(&ptr, &i, tid)) {
		dsPerror("dsTypeSpecifier failed");
		return FALSE;
	}
	printf("len: %d\n%s\n", i, ptr);
	printf("first tid %d\n", tid);
	if (!dsTypeId(&tid, str1, &ptr)) {
		dsPerror("dsTypeId failed");
		return FALSE;
	}
	printf("second tid %d\n", tid);
	if (!dsTypePtr(&type, tid)) {
		dsPerror("dsTypePtr failed");
		return FALSE;
	}
	for (i = 0; i < 1000; i++) {
		if (!dsTypeId(&n, str2, &ptr)) {
			dsPerror("dsTypeId failed");
			return FALSE;
		}
		if (n != tid) {
			printf("tidCmp failure %d != %d\n", n, tid);
			dsPerror("");
			return FALSE;
		}
	}
	dumpType(type);
	return TRUE;

}
/******************************************************************************
*
*/
int dsTestErr()
{
	size_t tid;

	dsPerror("before error");
	dsTypeId(&tid, "better be a syntax eror", NULL);
	dsPerror("after dsTypeId");
	printf("dsError %d\n", dsErrorCode());
	printf("dsPerror with NULL msg:\n");
	dsPerror(NULL);
	printf("dsPerror with null string msg:\n");
	dsPerror("");
	dsLogError(123, "test of dsLogError", __FILE__, __LINE__);
	dsPerror("after dsLogError");
	return TRUE;
}
/*****************************************************************************
*/
#define TEST_FAIL(b) {fprintf(stderr, "%s(%d) %s\n", __FILE__, __LINE__, #b);\
	dsPerror(""); return 0;}
#define F(b) {if(b)TEST_FAIL(b);}
#define T(b) {if(!(b))TEST_FAIL(b);}
/*****************************************************************************
*/
int dsTestGraph(void)
{
	char buf[200];
	DS_BUF_T bp;
	DS_DATASET_T *a, *b, *c, *d, *e;
	DS_LIST_T list;

	T(dsNewDataset(&a, "a"));
	T(dsLink(a,a));
	T(dsUnlink(a,a));
	T(dsNewDataset(&b, "b"));
	T(dsNewDataset(&c, "c"));
	T(dsNewDataset(&d, "d"));
	T(dsLink(a, b));
	T(dsLink(a, c));
	T(dsLink(a, d));
	T(dsLink(b, c));
	T(dsLink(b, d));
	T(dsLink(c, d));
	T(dsListInit(&list));
	T(dsVisitList(&list, a));
	T(dsListFree(&list));
	T(a->visit == 1);
	T(b->visit == 2);
	T(c->visit == 3);
	T(d->visit == 4);
	T(dsPrintDatasetSpecifier(stdout, a));
	T(dsIsAcyclic(a));
	T(dsLink(d, a));
	F(dsIsAcyclic(a));
	T(dsPrintDatasetSpecifier(stdout, a));
	DS_PUT_INIT(&bp, buf, sizeof(buf));
	T(dsDatasetSpecifier(&bp, a));
	T(dsCreateDataset(&e, NULL, &buf[4], NULL));
	T(dsPrintDatasetSpecifier(stdout, e));
	T(dsFreeDataset(e));
	T(dsFreeDataset(a));
	return TRUE;
}
/******************************************************************************
*
* dsTestTree - test dataset routines
*
*/
int dsTestTree()
{
	const char *ptr;
	DS_DATASET_T *pDataset;
	const char *typeDef1 = "struct type1 {long v1, v2;}";
	const char *typeDef2 = "struct tableType {long v1, v2; char name[20];}";
	const char *treeDef = "event{first{table1(type1,4000), table2(tableType, 0),"
		"sub{table3(type1,5)}}, second{t1(type1, 4),t2(type1, 5)},empty{}}";
	const char *table ="table(tableType, 7)";

	size_t typeList[256], *pList = typeList;

	if (!dsTypeListCreate(&pList, 256)) {
		printf("dsTypeListCreate Failed\n");
		return FALSE;
	}
	if (!dsTypeListEnter(typeList, typeDef1, &ptr)) {
		printf("dsTypeListEnter failed for typeDef1\n");
		return FALSE;
	}
	if (!dsTypeListEnter(typeList, typeDef2, &ptr)) {
		printf("dsTypeListEnter failed for typeDef2\n");
		return FALSE;
	}
	if (!dsCreateDataset(&pDataset, typeList, treeDef, &ptr)) {
		dsPerror("dsCreateDataset failed for dataset");
		return FALSE;
	}
	printf("dsCreateDataset success for dataset, ptr = %s\n", ptr);
	if (!dsPrintDatasetSpecifier(stdout, pDataset)) {
		dsPerror("dsPrintDatasetSpecifier failed for dataset");
		return FALSE;
	}
	if (!dsFreeDataset(pDataset)) {
		dsPerror("dsFreeDataset");
		return FALSE;
	}
	if (!dsCreateDataset(&pDataset, typeList, table, &ptr)) {
		dsPerror("dsCreateDataset failed for table");
		return FALSE;
	}
	printf("dsCreateDataset success for table\n");
	printf("ptr = %s\n", ptr);
	if (!dsPrintDatasetSpecifier(stdout, pDataset)) {
		dsPerror("dsPrintDatasetSpecifier failed for table");
		return FALSE;
	}
	if (!dsFreeDataset(pDataset)) {
		dsPerror("dsFreeDataset");
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* dsTestDset - simple tests of dataset routines
*
*/
int dsTestDset()
{
	char buf[10]; const char *ptr;
	char *tblDecl[] = {"struct type1 {short a;}", "struct type2 {long l;}",
		"struct type3 {float f;}", "struct type4 {unsigned long x;}"};
	int i, a[21];
	long l[5];
	char *pData[] = {NULL, NULL, NULL, NULL};
	size_t dim[] = {10, 5, 12, 31, 8, 4, 12, 2};
	DS_DATASET_T *pChild, *pParent, *pTable;

	if (!dsNewDataset(&pParent, "parent")) {
		dsPerror("NewDataset failed for parent");
		return FALSE;
	}
	if (!dsNewDataset(&pChild, "childDataset")) {
		dsPerror("NewDataset failed for child");
		return FALSE;
	}
	if (!dsLink(pParent, pChild)) {
		dsPerror("Link failed");
		return FALSE;
	}
	pData[0] = (char *)a;
	pData[1] = (char *)l;
	for (i = 0; i < 20; i++) {
		sprintf(buf, "tbl%d", i);
		ptr =pData[i%2];
		pTable = NULL;
		if (!dsNewTable(&pTable,
			buf, tblDecl[i%4], dim[i%8], &ptr)) {
			dsPerror("dsNewTable failed");
			return FALSE;
		}
		if (!dsLink(pParent, pTable)) {
			dsPerror("Link failed for pTable");
			return FALSE;
		}
	}
	if (!dsPrintDatasetSpecifier(stdout, pParent)) {
		dsPerror("dsPrintDatasetSpecifier failed");
		return FALSE;
	}
	if (!dsFreeDataset(pParent)) {
		dsPerror("dsFreeDataset failed");
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* dsTestType - simple test of abstract data type routines
*
*/
int dsTestType()
{
      const char *str =	"struct test {struct s {double d; long l;}h;\n"
			"\tstruct z {short v; struct s{char c;}r; long t;}a;\n"
			"\tstruct t {z r[5][20]; short y; octet z;}b;\n"
			"\tstruct m {t w; struct s{long l;}v;}u;\n"
			"\t m e;\n"
			"\tchar end;}\n";
     const char *str2 =	"struct  test  {struct s {double d; long l;}h;\n"
			"\tstruct z {short v; struct s{char c;}r; long t;}a;\n"
			"\tstruct t {z r[5][20]; short y; octet z;}b;\n"
			"\tstruct m {t w; struct\n"
			" s{long l;}v;}u;\n"
			"\t m e;\n"
			"\tchar end;}\n";
	const char *ptr;
	size_t i, n, tid1, tid2;
	DS_TYPE_T *type;

	T(dsTypeId(&tid1, "struct a{char a;struct d{char b;}c;}", &ptr))
	printf("%s", str);
	if (!dsTypeId(&tid1, str, &ptr)) {
		dsPerror("dsTypeId failed");
		return FALSE;
	}
	if (!dsTypeSpecifier(&ptr, &i, tid1)) {
		dsPerror("dsTypeSpecifier failed");
		return FALSE;
	}
	printf("len: %d\n%s\n", i, ptr);
	printf("first tid %d\n", tid1);
	if (!dsTypeId(&tid2, str, &ptr) || tid1 != tid2) {
		dsPerror("dsTypeId failed");
		return FALSE;
	}
	printf("second tid %d\n", tid2);
	if (!dsTypePtr(&type, tid2)) {
		dsPerror("dsTypePtr failed");
		return FALSE;
	}
	for (i = 0; i < 1000; i++) {
		if (!dsTypeId(&n, str2, &ptr)) {
			dsPerror("dsTypeId failed");
			return FALSE;
		}
		if (n != tid1) {
			printf("tidCmp failure %d != %d\n", n, tid1);
			dsPerror("");
			return FALSE;
		}
	}
	dumpType(type);
	return TRUE;

}
/******************************************************************************
*
* xdrMemTest - test xdr_dataset in memory region
*
*/
int xdrMemTest(void)
{
	size_t size;
	char *addr;
	XDR xdr;

	size = XDR_MEM_SIZE(NLOOP);
	printf("xdrMemTest: size %d\n", size);
	if ((addr = MALLOC(size)) == NULL) {
		printf("xdrMemTest - MALLOC(%d) failed \n", size);
		goto fail;
	}
	xdrmem_create(&xdr, addr, size, XDR_ENCODE);

	if (!dsWriteTest(&xdr, NLOOP)) {
		printf("xdrMemTest - xdr_write failed\n");
		goto fail;
	}
	printf("%d bytes\n\n", xdr_getpos(&xdr));
	xdrmem_create(&xdr, addr, size, XDR_DECODE);

	if (!dsReadTest(&xdr, NLOOP)) {
		printf("xdrMemTest - xdr_read failed\n");
		goto fail;
	}
	printf("%d bytes\n\n", xdr_getpos(&xdr));
	FREE(addr);
	return TRUE;
fail:
	if (addr != NULL) {
		FREE(addr);
	}
	return FALSE;
}
/******************************************************************************
*
* xdrReadTest - read data from file using xdr_dataset
*
*/
int xdrReadTest(int fast)
{
	FILE *stream;
	XDR xdr;

	if ((stream = fopen("xtest.bin", "rb")) == NULL) {
		printf("fopen failed for read\n");
		return FALSE;
	}
	xdrstdio_create(&xdr, stream, XDR_DECODE);
	if (fast) {
		dsReadAll(&xdr);
		return TRUE;
	}
	return dsReadTest(&xdr, NLOOP);

}
/******************************************************************************
*
* xdrWriteTest - write data to file using xdr_dataset
*
*/
int xdrWriteTest()
{
	FILE *stream;
	XDR xdr;

	/* write dataset to file */
	if ((stream = fopen("xtest.bin", "wb")) == NULL) {
		printf("fopen failed for write\n");
		return FALSE;
	}
	xdrstdio_create(&xdr, stream, XDR_ENCODE);
	return dsWriteTest(&xdr, NLOOP);
	/*
	return xdr_write(&xdr);
	*/
}
/******************************************************************************
*
*/
void testStats(void)
{
	printf("\n");
	dsAllocStats();
	dsTidHashStats();
	dsSemStats();
}
