/* Copyright 1993, Lawrence Berkeley Laboratory */

/* testlib.c - test abstract type functions */

/*
modification history
--------------------
26jul93,whg  made from testadt.c testhash.c testtree.c testxdr.c.
25feb95,whg  modify for CORBA IDL type system
*/

/*
DESCRIPTION
collection of routine to test ds lib
*/
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#define DS_PRIVATE
#include "dscodes.h"
#include "dstype.h"
#include "dsxdr.h"


#ifdef _MSDOS
#define NLOOP 20
#else
#define NLOOP 100
#endif
#define XDR_MEM_SIZE(nloop) ((nloop)*(300 + 40*(nloop)))
void dumpType(DS_TYPE_T *type);
static void dumpTypeR(DS_TYPE_T *type, char *prefix);
/******************************************************************************
*
* dsTestAdt - simple test of abstract data type routines
*
*/
int dsTestType()
{
	char *str =	"struct test {struct s {double d; long l;}h;\n"
			"\tstruct z {short v; struct s{char c;}r; long t;}a;\n"
			"\tstruct t {z r[5][20]; short y; octet z;}b;\n"
			"\tstruct m {t w; struct s{long l;}v;}u;\n"
			"\t m e;\n"
			"\tchar end;}\n";
	char *str2 =	"struct  test  {struct s {double d; long l;}h;\n"
			"\tstruct z {short v; struct s{char c;}r; long t;}a;\n"
			"\tstruct t {z r[5][20]; short y; octet z;}b;\n"
			"\tstruct m {t w; struct\n"
			" s{long l;}v;}u;\n"
			"\t m e;\n"
			"\tchar end;}\n";
	char *ptr;
	size_t i, n, tid;
	DS_TYPE_T *type;

	printf("%s", str);
	if (!dsTypeId(&tid, str, &ptr)) {
		dsPerror("dsTypeId failed");
		return FALSE;
	}
	if (!dsTypeSpecifier(&ptr, &i, tid)) {
		dsPerror("dsTypeSpecifier failed");
		return FALSE;
	}
	printf("len: %d\n%s\n", i, ptr);
	printf("first tid %d\n", tid);
	if (!dsTypeId(&tid, str, &ptr)) {
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
* dsTestCorba - simple test of abstract data type routines
*
*/
int dsTestCorba()
{
	char *str1 = "struct test {struct s {long x, y;}z;\n"
		"long l; struct k{struct s{double u, v;}news;}kk;\n"
		"short sh[10][12]; s q; k v;}";
	char buf[200], *ptr;
	size_t i, n, tid;
	DS_TYPE_T *type = NULL;

	char *str2 = str1;
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
*/
int dsTestErr()
{
	size_t tid;

	dsPerror("before error");
	dsTypeId(&tid, "better be a syntax eror", NULL);
	dsPerror("after dsTypeId");
	printf("dsError %d\n", dsError);
	printf("dsPerror with NULL msg:\n");
	dsPerror(NULL);
	printf("dsPerror with null string msg:\n");
	dsPerror("");
	dsClearError();
	dsPerror("after dsClearError");
	return TRUE;
}
/******************************************************************************
*
* dsTestTree - test dataset routines
*
*/
int dsTestTree()
{
	char *ptr;
	DS_DATASET_T dataset, *pDataset;
	char *typeDef1 = "struct type1 {long v1, v2;}";
	char *typeDef2 = "struct tableType {long v1, v2; char name[20];}";
	char *treeDef = "event{first{table1(type1,4000), table2(tableType, 0),"
		"sub{table3(type1,5)}}, second{t1(type1, 4),t2(type1, 5)},empty{}}";
	char *table ="table(tableType, 7)";

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
	pDataset = NULL;
	if (!dsCreateDataset(&pDataset, 0, typeList, treeDef, &ptr)) {
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
	pDataset = &dataset;
	if (!dsCreateDataset(&pDataset, 1, typeList, table, &ptr)) {
		dsPerror("dsCreateDataset failed for table");
		return FALSE;
	}
	printf("dsCreateDataset success for table\n");
	printf("ptr = %s\n", ptr);
	if (!dsPrintDatasetSpecifier(stdout, pDataset)) {
		dsPerror("dsPrintDatasetSpecifier failed for table");
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
	char buf[10], *ptr;
	char *tblDecl[] = {"struct type1 {short a;}", "struct type2 {long l;}",
		"struct type3 {float f;}", "struct type4 {unsigned long x;}"};
	int i, a[21];
	long l[5];
	char *pData[] = {NULL, NULL, NULL, NULL};
	size_t dim[] = {10, 5, 12, 31, 8, 4, 12, 2};
	DS_DATASET_T dataset[21], *pDataset = dataset;

	pData[0] = (char *)a;
	pData[1] = (char *)l;

	if (!dsNewDataset(&pDataset, "myDset", 21)) {
		printf("dsNewDataset failed\n");
		return FALSE;
	}
	for (i = 0; i < 20; i++) {
		sprintf(buf, "tbl%d", i);
		ptr =pData[i%2];
		if (!dsAddTable(pDataset,
			buf, tblDecl[i%4], dim[i%8], &ptr)) {
			dsPerror("dsAddTable failed");
			return FALSE;
		}
	}
	if (!dsPrintDatasetSpecifier(stdout, pDataset)) {
		dsPerror("dsPrintDatasetSpecifier failed");
		return FALSE;
	}
	dsFreeDataset(pDataset);
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
	if ((addr = malloc(size)) == NULL) {
		printf("xdrMemTest - malloc(%d) failed \n", size);
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
	free(addr);
	return TRUE;
fail:
	if (addr != NULL) {
		free(addr);
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
	dsDatasetAllocStats();
	dsTidHashStats();
	dsTypeLockStats();
}
