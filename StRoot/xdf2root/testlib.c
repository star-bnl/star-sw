/* Copyright 1993, Lawrence Berkeley Laboratory */

/* testlib.c - test abstract type functions */

/*
modification history
--------------------
01a,26jul93,whg  made from testadt.c testhash.c testtree.c testxdr.c.

*/

/*
DESCRIPTION
TBS ...
*/
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <rpc/rpc.h>

#define DS_PRIVATE
#include "dscodes.h"
#include "dstype.h"
#include "dsxdr.h"


#define NROW 100
#define NLOOP 40
#ifdef _MSDOS
#define XDR_MEM_SIZE 260
#else
#define XDR_MEM_SIZE 26000
#endif

#define byte signed char
#define u_byte unsigned char
#define u_short unsigned short
#define u_int unsigned
#define u_long unsigned long

char *def = "struct table_tag {char c; byte b; u_byte ub; short s;"
"u_short us; int i; u_int ui; long l; u_long ul; float f; double d;"
"short sa[5][20]; struct {float x; long n;}v;};";

struct table_tag {
	char c;
	byte b;
	u_byte ub;
	short s;
	u_short us;
	int i;
	u_int ui;
	long l;
	u_long ul;
	float f;
	double d;
	short sa[5][20];
	struct {float x; long n;}v;
};

int dsPrintType(DS_TYPE_T *type);
void dumpMem(char *ptr, int count);
void dumpType(DS_TYPE_T *type);
static void dumpTypeR(DS_TYPE_T *type, char *prefix);
static void setTable(struct table_tag *table);
int testStats(void);
int xdr_read(XDR *xdrs, int fast);
int xdr_write(XDR *xdrs);


/******************************************************************************
*
*/
int dsBug()
{
	/* put code here */
	return TRUE;
}
/******************************************************************************
*
* dsTestAdt - simple test of abstract data type routines
*
*/
int dsTestAdt()
{
	char *str =	"struct test {struct s {double d; long l;}h;\n"
			"\tstruct  {short v; struct s r; long t;}a;\n"
			"\tstruct t {struct s r[5][20]; short y; byte z;}b;\n"
			"\tstruct m {struct t w; struct s v;}u;\n"
			"\tstruct m e;\n"
			"\tchar end;}\n";
	char *str2 =	"struct  test  {struct s {double d; long l;}h;\n"
			"\tstruct  {short v; struct s r; long t;}a;\n"
			"\tstruct t {struct s r[5][20]; short y; byte z;}b;\n"
			"\tstruct m {struct t w;\n"
			"\tstruct s v;}u;\n"
			"\tstruct m e;\n"
			"\tchar end;}\n";
	char *ptr;
	size_t i, n, tid;
	DS_TYPE_T *type;

	printf("%s", str);
	if (!dsTypeId(&tid, str, &ptr)) {
		dsPerror("dsTypeId failed");
		return FALSE;
	}
	if (!dsTypeDef(&ptr, &i, tid)) {
		dsPerror("dsTypeDef failed");
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
	testStats();
	return TRUE;

}
/******************************************************************************
*
* dumpType - print info about type
*
*/
void dumpType(type)
DS_TYPE_T *type;
{
	printf(" offset  count std type\n\n");
	dumpTypeR(type, "");
}
/******************************************************************************
*
* dumpTypeR - recursive part of dumpType
*
*/
static void dumpTypeR(type, prefix)
DS_TYPE_T *type;
char *prefix;
{
	char s[100];
	size_t i, j;
	DS_FIELD_T *field;

	for (field = type->field, i = 0; i < type->nField; i++, field++) {
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
* dsTestMisc - dump basic type struct and simple test of type parse
*
*/
int dsTestMisc()
{
	char *ptr;
	size_t size;
	DS_TYPE_T *type = NULL;

	if (!dsCreateType(&type, &size, def, &ptr)) {
		dsPerror("dsCreateType failed");
		return FALSE;
	}
	printf("typeSize %d\n", size);
	dsPrintType(type);
	dsDumpTypes();
	free((char *)type);
	testStats();
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
	char *typeDef1 = "struct type1 {int v1, v2;}";
	char *typeDef2 = "struct tableType {int v1, v2; char name[20];}";
	char *treeDef = "event{first{table1(type1,4000), table2(),"
		"sub{table3(), table4()}}, second{t1(),t2(),t3(),t4()}}";
	char *table ="table(tableType)";

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
	if (!dsPrintDataset(pDataset)) {
		dsPerror("dsPrintDataset failed for dataset");
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
	if (!dsPrintDataset(pDataset)) {
		dsPerror("dsPrintDataset failed for table");
		return FALSE;
	}
	testStats();
	return TRUE;
}
/******************************************************************************
*
* dsTestDset - simple tests of table set routines
*
*/
int dsTestDset()
{
	char buf[10];
	char *tblDecl[] = {"struct type1 {int a;}", "struct type2 {long l;}",
		"struct type3 {float f;}", "struct type4 {int x;}"};
	int i, a[21];
	long l[5];
	char *pData[] = {NULL, NULL, NULL, NULL};
	size_t dim[] = {10, 5, 12, 31};
	DS_DATASET_T dataset[21], *pDataset = dataset;

	pData[0] = (char *)a;
	pData[1] = (char *)l;

	if (!dsNewDataset(&pDataset, "myDset", 21)) {
		printf("dsNewDataset failed\n");
		return FALSE;
	}
	for (i = 0; i < 20; i++) {
		sprintf(buf, "tbl%d", i);
		if (!dsAddTable(pDataset,
			buf, tblDecl[i%4], dim[i%4], &pData[i%4], NULL)) {
			dsPerror("dsAddTable failed");
			return FALSE;
		}
	}
	if (!dsPrintDataset(pDataset)) {
		dsPerror("dsPrintDataSet failed");
		return FALSE;
	}
	testStats();
	return TRUE;
}
/******************************************************************************
*
* print offsets
*
*/
void printOffset(void)
{
	printf("offsetof\tb\t%d\n", offsetof(struct table_tag, b));
	printf("offsetof\tub\t%d\n", offsetof(struct table_tag, ub));
	printf("offsetof\ts\t%d\n", offsetof(struct table_tag, s));
	printf("offsetof\tus\t%d\n", offsetof(struct table_tag, us));
	printf("offsetof\ti\t%d\n", offsetof(struct table_tag, i));
	printf("offsetof\tui\t%d\n", offsetof(struct table_tag, ui));
	printf("offsetof\tl\t%d\n", offsetof(struct table_tag, l));
	printf("offsetof\tul\t%d\n", offsetof(struct table_tag, ul));
	printf("offsetof\tf\t%d\n", offsetof(struct table_tag, f));
	printf("offsetof\td\t%d\n", offsetof(struct table_tag, d));
	printf("offsetof\tsa\t%d\n", offsetof(struct table_tag, sa));
	printf("offsetof\tv\t%d\n", offsetof(struct table_tag, v));
	printf("sizeof  table_tag\t%d\n", sizeof(struct table_tag));
	
}
/******************************************************************************
*
* checkDataset - verify data read by xdr_dataset
*
*/
int checkDataset(DS_DATASET_T *dataset)
{
	int i, j, r;
	struct table_tag *table = (struct table_tag *)dataset->p.data;
	DS_TYPE_T *type;

	if (dataset->elcount != NROW) {
		printf("checkDataset bad elcount %d", dataset->elcount);
		return FALSE;
	}
	for (r = 0; r < NROW; r++) {
		if (table[r].c  != 0 ||
			table[r].b  != 1 ||
			table[r].ub != 2 ||
			table[r].s  != 3 ||
			table[r].us != 4 ||
			table[r].i  != 5 ||
			table[r].ui != 6 ||
			table[r].l  != 7 ||
			table[r].ul != 8 ||
			table[r].f  != 9 ||
			table[r].d  != 10) {
			printf("compare failed for basic types, row %d\n", r);
			printf("%d %d %d %d %d %d %d %ld %ld %g %g\n", table[r].c, table[r].b,
				table[r].ub, table[r].s, table[r].us, table[r].i, table[r].ui,
				table[r].l, table[r].ul, table[r].f, table[r].d);
			dumpMem((char *)&table[r], sizeof(struct table_tag));
			if (dsTypePtr(&type, dataset->tid)) {
				dumpType(type);
			}
			printOffset();
			return FALSE;
		}
		if ( table[r].v.x != 3.0 ||
			table[r].v.n != 1234) {
			printf("compare failed for struct types, row %d\n", r);
			return FALSE;
		}
		for (i = 0; i < 5; i++) {
			for (j = 0; j < 20; j++) {
				if (table[r].sa[i][j] != (i + 5*j)) {
					printf("compare failed for array, row %d\n", r);
					return FALSE;
				}
			}
		}
	}
	return TRUE;
}
/******************************************************************************
*
* dumpMem - print contents of memory
*
*/
void dumpMem(char *ptr, int count)
{
	unsigned char *ucp = (unsigned char *)ptr;
	int i;
	 
	printf("offset %X\n", (size_t)ptr);
	for (i = 0; i < count; i++) {
		if (i%16 == 0) {
			printf("\n%.6i ", i);
		}
		printf(" %.2X", ucp[i]);
	}
	printf("\n");
}
/******************************************************************************
*
* setTable - set values for table written by xdr_dataset
*
*/
static void setTable(struct table_tag *table)
{
	int i, j, r;
    
    memset((char *)table, 0, NROW*sizeof(struct table_tag));
	for (r = 0; r < NROW; r++) {
		table[r].c  = 0;
		table[r].b  = 1;
		table[r].ub = 2;
		table[r].s  = 3;
		table[r].us = 4;
		table[r].i  = 5;
		table[r].ui = 6;
		table[r].l  = 7;
		table[r].ul = 8;
		table[r].f  = (float)9;
		table[r].d  = 10;
		table[r].v.x = 3.0f;
		table[r].v.n = 1234;
	
		for (i = 0; i < 5; i++) {
			for (j = 0; j < 20; j++) {
				table[r].sa[i][j] = i + 5*j;
			}
		}
	}
}
/******************************************************************************
*
* dsPrintType - print type declaration in standard form
*
*/
int dsPrintType(DS_TYPE_T *type)
{
	char buf[500];

	if (!dsFmtTypeDef(buf, sizeof(buf), type)) {
		dsPerror("dsFmtTypeDef failed");
		return FALSE;
	}
	printf("%s", buf);
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

	size = NLOOP*XDR_MEM_SIZE;
	if ((addr = malloc(size)) == NULL) {
		printf("xdrMemTest - malloc(%d) failed \n", size);
		goto fail;
	}
	xdrmem_create(&xdr, addr, size, XDR_ENCODE);

	if (!xdr_write(&xdr)) {
		printf("xdrMemTest - xdr_write failed\n");
		goto fail;
	}

	xdrmem_create(&xdr, addr, size, XDR_DECODE);

	if (!xdr_read(&xdr, 0)) {
		printf("xdrMemTest - xdr_read failed\n");
		goto fail;
	}
	free(addr);
	testStats();
	return TRUE;
fail:
	if (addr != NULL) {
		free(addr);
	}
	testStats();
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
	return xdr_read(&xdr, fast);
}
/******************************************************************************
*
* xdr_read - read from xdr stream using xdr_dataset
*
*/
int xdr_read(XDR *xdrs, int fast)
{
	size_t l;
	DS_DATASET_T *dataset;

	for(l = 0; l < NLOOP; l++) {
		dataset = NULL;
		if (!xdr_dataset(xdrs, &dataset)) {
			dsPerror("xdr_dataset failed for DECODE");
			return FALSE;
		}
		if (!fast) {
			if (!checkDataset(dataset)) {
				printf("record %d\n", l);
				return FALSE;
			}
		}
		xdr_free(xdr_dataset, (char *)&dataset);
		if (dataset != NULL) {
			printf("xdr_free failed\n");
			return FALSE;
		}
	}
	printf("DECODE success\n");
	testStats();
	return TRUE;
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
	return xdr_write(&xdr);
}
/******************************************************************************
*
* xdr_write - write test data using xdr_dataset
*
*/
int xdr_write(XDR *xdrs)
{
	char *ptr;
	size_t l, tid;
	struct table_tag *table;
	DS_DATASET_T dataset, *pDataset = &dataset;

	/* create dataset */

	if (!dsTypeId(&tid, def, &ptr)) {
		dsPerror("writeTest: sdStrToTid failed");
		return FALSE;
	}
	table = (struct table_tag *)malloc(NROW*sizeof(struct table_tag));
	if (table == NULL) {
		printf("malloc failed for ENCODE\n");
		return FALSE;
	}
	setTable(table);
	memset((char *)&dataset, 0, sizeof(dataset));
	strcpy(dataset.name, "xdrDataset");
	dataset.tid = tid;
	dataset.elcount = NROW;
	dataset.p.data = (void *)table;

	for (l = 0; l < NLOOP; l++) {
		if (!xdr_dataset(xdrs, &pDataset)) {
			free(table);
			dsPerror("xdr_dataset failed for ENCODE");
			return FALSE;
		}
	}
	free(table);
	printf("ENCODE success\n");
	testStats();
	return TRUE;
}
/******************************************************************************
*
*/
int testStats(void)
{
	dsDatasetAllocStats();
	dsTidHashStats();
	dsTypeLockStats();
	return TRUE;
}
