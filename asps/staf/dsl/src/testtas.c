/* Copyright 1994, Lawrence Berkeley Laboratory */

/* testtas.c - test prototype tas to ds interface */

/*
modification history
--------------------
13apr94,whg	written.
11feb95,whg	new tasProject tests
*/

/*
DESCRIPTION
TBS ...
*/
#include <string.h>
#define DS_PRIVATE
#include "dsxdr.h"

typedef struct base_type {float x, y, z;}BASE_TYPE;
#define BASE_DECL "struct base_type {float x, y, z;}"

typedef struct differ_type {float x, y; long z;}DIFFER_TYPE;
#define DIFFER_DECL "struct differ_type {float x, y; long z;}"

typedef struct fewer_type {float x, y;}FEWER_TYPE;
#define FEWER_DECL "struct fewer_type {float x, y;}"

typedef struct more_type {float x, y, z; long t;}MORE_TYPE;
#define MORE_DECL "struct more_type {float x, y, z; long t;}"

typedef struct none_type {long x, y, z;}NONE_TYPE;
#define NONE_DECL "struct none_type {long x, y, z;}"

typedef struct order_type {float z, y, x;}ORDER_TYPE;
#define ORDER_DECL "struct order_type {float z, y, x;}"

#define DSET_DIM 10
#define TABLE_DIM 20

#define LONG_LEN TABLE_DIM
#define BASE_LEN LONG_LEN - 10


int readFile(char *fileName);
int writeFile(char *fileName);
int xdrCreate(XDR *xdrs, char *fileName, int op);
/******************************************************************************
*/
int projectTest()
{
	char *fileName = "tas.bin";

	if (!writeFile(fileName) || !readFile(fileName)){
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*/
int checkBase(BASE_TYPE *base, size_t nRow)
{
	size_t i;
	
	for (i = 0; i < nRow; i++) {
		if (base[i].x != (float)(100*i +10) ||
			base[i].y != (float)(100*i +20) ||
			base[i].z != (float)(100*i +30)) {
			printf("base[%d] x %f, y %f, z %f\n",
				i, base[i].x, base[i].y, base[i].z);
			return FALSE;
		}
	}
	return TRUE;
}
/******************************************************************************
*/
int checkDiffer(DIFFER_TYPE *differ, size_t nRow)
{
	size_t i;
	
	for (i = 0; i < nRow; i++) {
		if (differ[i].x != (float)(100*i +10) ||
			differ[i].y != (float)(100*i +20) ||
			differ[i].z != 0) {
			printf("differ[%d] x %f, y %f, z %ld\n",
				i, differ[i].x, differ[i].y, differ[i].z);
			return FALSE;
		}
	}
	return TRUE;
}
/******************************************************************************
*/
int checkFewer(FEWER_TYPE *fewer, size_t nRow)
{
	size_t i;
	
	for (i = 0; i < nRow; i++) {
		if (fewer[i].x != (float)(100*i +10) ||
			fewer[i].y != (float)(100*i +20)) {
			printf("fewer[%d] x %f, y %f\n", i, fewer[i].x, fewer[i].y);
			return FALSE;
		}
	}
	return TRUE;
}
/******************************************************************************
*/
int checkMore(MORE_TYPE *more, size_t nRow)
{
	size_t i;
	
	for (i = 0; i < nRow; i++) {
		if (more[i].x != (float)(100*i +10) ||
			more[i].y != (float)(100*i +20) ||
			more[i].z != (float)(100*i +30)	||
			more[i].t != 0) {
			printf("more[%d] x %f, y %f, z %f, t %ld\n",
				i, more[i].x, more[i].y, more[i].z, more[i].t);
			return FALSE;
		}
	}
	return TRUE;
}
 
/******************************************************************************
*/
int checkOrder(ORDER_TYPE *order, size_t nRow)
{
	size_t i;
	
	for (i = 0; i < nRow; i++) {
		if (order[i].x != (float)(100*i +10) ||
			order[i].y != (float)(100*i +20) ||
			order[i].z != (float)(100*i +30)) {
			printf("order[%d] x %f, y %f, z %f\n",
				i, order[i].x, order[i].y, order[i].z);
			return FALSE;
		}
	}
	return TRUE;
}

/******************************************************************************
*/
int readFile(char *fileName)
{
	size_t count;
	BASE_TYPE base[TABLE_DIM], *pBase = base;
	DIFFER_TYPE differ[TABLE_DIM], *pDiffer = differ;
	FEWER_TYPE fewer[TABLE_DIM], *pFewer = fewer;
	MORE_TYPE more[TABLE_DIM], *pMore = more;
	ORDER_TYPE order[TABLE_DIM], *pOrder = order;
	DS_DATASET_T *pDataset = NULL;
	XDR xdr;

	if (!xdrCreate(&xdr, fileName, XDR_DECODE) ||
		!xdr_dataset(&xdr, &pDataset)){
		dsPerror("readFile: xdr failed\n");
		goto fail;
	}
	count = TABLE_DIM;
	memset(base, 127, sizeof(base));
	if (!dsTasProject(pDataset, "base", BASE_DECL, &count, &pBase)
		|| count != BASE_LEN || !checkBase(base, count)) { 
		printf("count %d\n", count);
		dsPerror("UNEXPECTED failue for base\n");
		goto fail;
	}
	/* check reorder of columns */
	memset(order, 127, sizeof(order));
	if (!dsTasProject(pDataset, "base", ORDER_DECL, &count, &pOrder) ||
		count != BASE_LEN || !checkOrder(order, count)) {
		printf("count %d\n", count);
		dsPerror("UNEXPECTED failure for order test\n");
		goto fail;
	}
	/* check project into fewer cols */
	memset(fewer, 127, sizeof(fewer));
	if (!dsTasProject(pDataset, "base", FEWER_DECL, &count, &pFewer) ||
		count != BASE_LEN || !checkFewer(fewer, count)) {
		printf("count %d\n", count);
		dsPerror("UNEXPECTED failure for extra columns\n");
		goto fail;
	}
	/* check project into more columns */
	memset(more, 127, sizeof(more));
	if (dsTasProject(pDataset, "base", MORE_DECL, &count, &pMore) ||
		count != BASE_LEN || !checkMore(more, count)) {
		printf("count %d\n", count);
		dsPerror("UNEXPECTED failure for missing columns test\n");
		goto fail;
	}
	dsPerror("missing columns test");
		
	/* check truncation */
	count = 3;
	memset(base, 127, sizeof(base));
	if (dsTasProject(pDataset, "long", BASE_DECL, &count, &pBase) ||
		count != 3 || !checkBase(base, count)) {
		printf("UNEXPECTED success for truncation test\n");
		goto fail;
	}
	dsPerror("truncation test");
	
	/* check project into different type column */
	count = TABLE_DIM;
	memset(differ, 127, sizeof(differ));
	if (dsTasProject(pDataset, "base", DIFFER_DECL, &count, &pDiffer) ||
		count != BASE_LEN || !checkDiffer(differ, count)) {
		printf("count %d\n", count);
		dsPerror("UNEXPECTED failure for differ test\n");
		goto fail;
	}
	dsPerror("column types differ test");

	/* multiple error test */
	count = 3;
	memset(base, 127, sizeof(base));
	if (dsTasProject(pDataset, "long", DIFFER_DECL, &count, &pDiffer) ||
		count != 3 || !checkDiffer(differ, count)) {
		printf("UNEXPECTED success for multiple failure test\n");
		goto fail;
	}
	dsPerror("multiple failure test");

	/* no matching columns */
	count = TABLE_DIM;
	if (dsTasProject(pDataset, "base", NONE_DECL, &count, &pBase) || 
		count != 0) {
		printf("count %d\n", count);
		printf("UNEXPECTED success for no matching columns test\n");
		goto fail;
	}
	dsPerror("no matching columns test");
	
	/* missing table test */ 
	count = TABLE_DIM;
	if (dsTasProject(pDataset, "missing", BASE_DECL, &count, &pBase) ||
		count != 0) {
		printf("count %d\n", count);
		printf("UNEXPECTED success for missing table test\n");
		goto fail;
	}	
	dsPerror("missing table test");
	
	/* bad type test */
	if (dsTasProject(pDataset, "base", "struct x{less x;}",	&count, &pBase)) {
		printf("UNEXPECTED success for bad type test\n");
		goto fail;
	}	
	dsPerror("bad type test");
	
	dsFreeDataset(pDataset);
	return TRUE;
fail:
	dsFreeDataset(pDataset);
	return FALSE;
}
/******************************************************************************
*/
int writeFile(char *fileName)
{
	char *pBase;
	size_t i;
	BASE_TYPE base[TABLE_DIM];
	DS_DATASET_T *pSrcDset;
	XDR xdr;


	for (i = 0; i < TABLE_DIM; i++) {
		base[i].x = (float)(100*i +10);
		base[i].y = (float)(100*i +20);
		base[i].z = (float)(100*i +30);
	}
	pBase = (char *)base;
	if (
		!dsNewDataset(&pSrcDset, "data") ||
		!dsAddTable(pSrcDset, "base", BASE_DECL, BASE_LEN, &pBase) ||
		!dsAddTable(pSrcDset, "long", BASE_DECL, LONG_LEN, &pBase) ||
		!dsAddTable(pSrcDset, "zero", BASE_DECL, 0, &pBase)
	){
		dsPerror("writeFile create dataset failed");
		return FALSE;
	}
	if (!xdrCreate(&xdr, fileName, XDR_ENCODE) ||
		!xdr_dataset(&xdr, &pSrcDset)
	){
		dsPerror("writeFile: xdr failed\n");
		return FALSE;
	}
	if (!dsFreeDataset(pSrcDset)) {
		dsPerror("writeFile free failed");
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* xdrCreate - open file and create XDR structure
*
*/
int xdrCreate(XDR *xdrs, char *fileName, int op)
{
	char *type;
	static FILE *stream = NULL;

	if (stream != NULL) {
		fclose(stream);
	}
	type = op == XDR_ENCODE ? "wb" : "rb";
	if ((stream = fopen(fileName, type)) == NULL) {
		dsErrorPrint("fopen(%s, %s) failed\n", fileName, type);
		return FALSE;
	}
	xdrstdio_create(xdrs, stream, op);
	return TRUE;
}
