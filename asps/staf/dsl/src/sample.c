/* Copyright 1993, Lawrence Berkeley Laboratory */

/* sample.c - example data structure I/O */

/*
modification history
--------------------
01a,31jul93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <stdio.h>
#include <stdlib.h>
#include <rpc/rpc.h>

#define DS_PRIVATE

#include "dstype.h"
#include "dsxdr.h"
#include "sample.h"

int xdrCreate(XDR *xdrs, char *fileName, int op);
int readDynamic(XDR *xdrs);
int readVars(XDR *xdrs);
int writeDynamic(XDR *xdrs);
int writeProg(XDR *xdrs);
/******************************************************************************
*
* example data structure I/O
*
*/
void main(int argc, char **argv)
{
	char *file = "xtest.bin", *ptr = argv[1];
	int (*fcn)(XDR *xdrs), i, n = 3, op;
	XDR xdr;

	switch (argc == 2 && ptr[1] == '\0' ? *ptr : '?') {
	case 'd':
		fcn = readDynamic;
		op = XDR_DECODE;
		break;

	case 'r':
		fcn = readVars;
		op = XDR_DECODE;
		break;

	case 'v':
		fcn = writeDynamic;
		op = XDR_ENCODE;
		break;

	case 'w':
		fcn = writeProg;
		op = XDR_ENCODE;
		break;

	default:
		printf("sample d | r | v | w\n");
		printf("\td - read to dynamic memory\n");
		printf("\tr - read to program variables\n");
		printf("\tv - write from dynamic memory, type B table5\n");
		printf("\tw - write from program variables, type A table5\n");
		exit(0);
	}
	if (xdrCreate(&xdr, file, op)) {
		for (i = 0; i < n; i++) {
			if (!fcn(&xdr)) {
				break;
			}
		}
		printf("\nsample %s - %s\n\n",
			ptr, i == n ? "success" : "failure");
	}
	dsDatasetAllocStats();
	dsTypeLockStats();
	dsTidHashStats();
}
/******************************************************************************
*
* xdrCreate - open file and create XDR structure
*
*/
int xdrCreate(XDR *xdrs, char *fileName, int op)
{
	char *type;
	FILE *stream;

	type = op == XDR_ENCODE ? "wb" : "rb";
	if ((stream = fopen(fileName, type)) == NULL) {
		printf("fopen(%s, %s) failed\n", fileName, type);
		return FALSE;
	}
	xdrstdio_create(xdrs, stream, op);
	return TRUE;
}
/******************************************************************************
*
* checkTable5 - find table5 and check it - may be TYPE_A or TYPE_B
*/
int checkTable5(DS_DATASET_T *pDataset)
{
	char *typeStr;
	void *table5 = NULL;
	size_t nRow5;

	if (dsMapTable(pDataset, "table5", TYPE_A_S, &nRow5, &table5, NULL)) {
		typeStr = TYPE_A_S;
		printf("table5 is TYPE_A\n");
	}
	else if (dsMapTable(pDataset, "table5", TYPE_B_S, &nRow5, &table5, NULL)) {
		typeStr = TYPE_B_S;
		printf("table5 is TYPE_B\n");
	}
	else {
		dsPerror("checkTable5 map failed");
		return FALSE;
	}
	if (!checkTable(table5, typeStr, nRow5, NROW_TABLE5)) {
		printf("checkTable5 check failed\n");
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* readDynamic - read into dynamic memory
*
*/
int readDynamic(XDR *xdrs)
{
	TYPE_A_T *table1 = NULL;
	TYPE_B_T *table2 = NULL;
	TYPE_C_T *table3 = NULL;
	TYPE_D_T *table4 = NULL;
	size_t nRow1, nRow2, nRow3, nRow4;
	DS_DATASET_T *pDataset = NULL;

	/* read dataset and set table row counts and pointers */
	if (
		!xdr_dataset(xdrs, &pDataset) ||
		!dsMapTable(pDataset, "table1", TYPE_A_S, &nRow1, &table1, NULL) ||
		!dsMapTable(pDataset, "table2", TYPE_B_S, &nRow2, &table2, NULL) ||
		!dsMapTable(pDataset, "table3", TYPE_C_S, &nRow3, &table3, NULL) ||
		!dsMapTable(pDataset, "table4", TYPE_D_S, &nRow4, &table4, NULL)
	) {
		dsPerror("readDynamic failed");
		goto fail;
	}
	/* check data */
	if (
		!checkTable(table1, TYPE_A_S, nRow1, NROW_TABLE1) ||
		!checkTable(table2, TYPE_B_S, nRow2, NROW_TABLE2) ||
		!checkTable(table3, TYPE_C_S, nRow3, NROW_TABLE3) ||
		!checkTable(table4, TYPE_D_S, nRow4, NROW_TABLE4) ||
		!checkTable5(pDataset)
	) {
		goto fail;
	}

	xdr_free(xdr_dataset, (char *)&pDataset);
	return TRUE;

fail:
	if (pDataset != NULL) {
		xdr_free(xdr_dataset, (char *)&pDataset);
	}
	return FALSE;
}
/******************************************************************************
*
* readVars - read dataset into program variables
*
*/
int readVars(XDR *xdrs)
{
	TYPE_A_T table1[NROW_TABLE1], *pTable1 = table1;
	TYPE_B_T table2[NROW_TABLE2], *pTable2 = table2;
	TYPE_C_T table3[NROW_TABLE3], *pTable3 = table3;
	TYPE_D_T table4[NROW_TABLE4], *pTable4 = table4;
	size_t nRow1 = NROW_TABLE1, nRow2 = NROW_TABLE2;
	size_t nRow3 = NROW_TABLE3, nRow4 = NROW_TABLE4;
	DS_DATASET_T dataset[DSET_DIM], *pDataset = dataset;

	/* read dataset into program vars */
	if (
		!xdr_dataset_type(xdrs, &pDataset, DSET_DIM) ||
		!dsMapTable(pDataset, "table1", TYPE_A_S, &nRow1, &pTable1, NULL) ||
		!dsMapTable(pDataset, "table2", TYPE_B_S, &nRow2, &pTable2, NULL) ||
		!dsMapTable(pDataset, "table3", TYPE_C_S, &nRow3, &pTable3, NULL) ||
		!dsMapTable(pDataset, "table4", TYPE_D_S, &nRow4, &pTable4, NULL) ||
		!dsAllocTables(pDataset) ||
		!xdr_dataset_data(xdrs, pDataset)
	) {
		dsPerror("readVars failed");
		goto fail;
	}
	/* check data */
	if (
		!checkTable(table1, TYPE_A_S, nRow1, NROW_TABLE1) ||
		!checkTable(table2, TYPE_B_S, nRow2, NROW_TABLE2) ||
		!checkTable(table3, TYPE_C_S, nRow3, NROW_TABLE3) ||
		!checkTable(table4, TYPE_D_S, nRow4, NROW_TABLE4) ||
		!checkTable5(pDataset)
	) {
		goto fail;
	}
	xdr_free(xdr_dataset, (char *)&pDataset);
	return TRUE;

fail:
	xdr_free(xdr_dataset, (char *)&pDataset);
	return FALSE;
}
/******************************************************************************
*
* writeDynamic - allocate memory and write dataset
*
*/
int writeDynamic(XDR *xdrs)
{
	DS_DATASET_T *pDataset = NULL;

	if (
		!dsNewDataset(&pDataset, "example", DSET_DIM) ||
		!dsAddTable(pDataset, "table1", TYPE_A_S, NROW_TABLE1, NULL, NULL) ||
		!dsAddTable(pDataset, "table2", TYPE_B_S, NROW_TABLE2, NULL, NULL) ||
		!dsAddTable(pDataset, "table3", TYPE_C_S, NROW_TABLE3, NULL, NULL) ||
		!dsAddTable(pDataset, "table4", TYPE_D_S, NROW_TABLE4, NULL, NULL) ||
		!dsAddTable(pDataset, "table5", TYPE_B_S, NROW_TABLE5, NULL, NULL) ||
		!setDataset(pDataset)
	) {
		dsPerror("writeDynamic setup failed");
		goto fail;
	}
	/* write tables */
	if (!xdr_dataset(xdrs, &pDataset)) {
		dsPerror("writeDynamic xdr_dataset failed");
		goto fail;
	}
	xdr_free(xdr_dataset, (char *)&pDataset);
	return TRUE;

fail:
	if (pDataset != NULL) {
		xdr_free(xdr_dataset, (char *)&pDataset);
	}
	return FALSE;
}
/******************************************************************************
*
* writeProg - write dataset from program variables
*
*/
int writeProg(XDR *xdrs)
{
	TYPE_A_T table1[NROW_TABLE1], *pTable1 = table1;
	TYPE_B_T table2[NROW_TABLE2], *pTable2 = table2;
	TYPE_C_T table3[NROW_TABLE3], *pTable3 = table3;
	TYPE_D_T table4[NROW_TABLE4], *pTable4 = table4;
	TYPE_A_T table5[NROW_TABLE5], *pTable5 = table5;
	size_t nRow1 = NROW_TABLE1, nRow2 = NROW_TABLE2;
	size_t nRow3 = NROW_TABLE3, nRow4 = NROW_TABLE4;
	size_t nRow5 = NROW_TABLE5;
	DS_DATASET_T dataset[DSET_DIM], *pDataset = dataset;

	/* put test data in tables */
	if (
		!setTable(table1, TYPE_A_S, nRow1) ||
		!setTable(table2, TYPE_B_S, nRow2) ||
		!setTable(table3, TYPE_C_S, nRow3) ||
		!setTable(table4, TYPE_D_S, nRow4) ||
		!setTable(table5, TYPE_A_S, nRow5) 
	){
		return FALSE;
	}
	/* build and write dataset */
	if (
		!dsNewDataset(&pDataset, "example", DSET_DIM) ||
		!dsAddTable(pDataset, "table1", TYPE_A_S, nRow1, &pTable1, NULL) ||
		!dsAddTable(pDataset, "table2", TYPE_B_S, nRow2, &pTable2, NULL) ||
		!dsAddTable(pDataset, "table3", TYPE_C_S, nRow3, &pTable3, NULL) ||
		!dsAddTable(pDataset, "table4", TYPE_D_S, nRow4, &pTable4, NULL) ||
		!dsAddTable(pDataset, "table5", TYPE_A_S, nRow5, &pTable5, NULL) ||
		!xdr_dataset(xdrs, &pDataset)
	) {
		dsPerror("writeProg failed");
		return FALSE;
	}
	return TRUE;
}
