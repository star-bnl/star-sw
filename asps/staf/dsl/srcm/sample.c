/* Copyright 1993, Lawrence Berkeley Laboratory */

/* sample.c - example data structure I/O */

#ifndef WIN32
/*
modification history
--------------------
31jul93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <stdlib.h>
#define DS_PRIVATE
#include "dsxdr.h"
#include "sample.h"

/******************************************************************************
*/
#ifdef SAMPLE_MAIN
void main()
{
	sample();
}
#endif
/******************************************************************************
*
* sample data structure I/O
*
*/
int sample(void)
{
	if (writeVars() &&
		readVars() &&
		readDynamic() &&
		readProject() &&
		writeDynamic() &&
		readVars() &&
		readProject() &&
		readDynamic()) {
		printf("All tests OK\n");
		return TRUE;
	}
	else {
		printf("A test FAILED\n");
		return FALSE;
	}
}
/******************************************************************************
*
* checkTable5 - find table5 and check it - may be TYPE_A or TYPE_B
*/
int checkTable5(DS_DATASET_T *pDataset)
{
	char *typeStr;
	char *table5 = NULL;
	size_t nRow5;

	if (dsMapTable(pDataset, "table5", TYPE_A_S, &nRow5, &table5)) {
		typeStr = TYPE_A_S;
		printf("table5 is TYPE_A\n");
	}
	else if (dsMapTable(pDataset, "table5", TYPE_B_S, &nRow5, &table5)) {
		typeStr = TYPE_B_S;
		printf("table5 is TYPE_B\n");
	}
	else {
		dsPerror("checkTable5 map failed");
		return FALSE;
	}
	if (!dsCheckTable(table5, typeStr, nRow5, NROW_TABLE5)) {
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
int readDynamic(void)
{
	TYPE_A_T *table1 = NULL;
	TYPE_B_T *table2 = NULL;
	TYPE_C_T *table3 = NULL;
	TYPE_D_T *table4 = NULL;
	size_t nRow1, nRow2, nRow3, nRow4;
	DS_DATASET_T *pDataset = NULL;
	FILE *stream;
	XDR xdr;
	int rtn;
	
	/* open file and initialize xdr structure */
	if ((stream = fopen(SAMPLE_FILE, "rb")) == NULL) {
		printf("readDynamic: fopen failed\n");
		return FALSE;
	}
	xdrstdio_create(&xdr, stream, XDR_DECODE);

	/* read dataset and set table row counts and pointers */
	if (
		!xdr_dataset(&xdr, &pDataset) ||
		!dsMapTable(pDataset, "table1", TYPE_A_S, &nRow1, (char **)&table1) ||
		!dsMapTable(pDataset, "table2", TYPE_B_S, &nRow2, (char **)&table2) ||
		!dsMapTable(pDataset, "table3", TYPE_C_S, &nRow3, (char **)&table3) ||
		!dsMapTable(pDataset, "table4", TYPE_D_S, &nRow4, (char **)&table4)
	) {
		dsPerror("readDynamic failed");
		goto fail;
	}
	/* check data */
	if (
		!dsCheckTable(table1, TYPE_A_S, nRow1, NROW_TABLE1) ||
		!dsCheckTable(table2, TYPE_B_S, nRow2, NROW_TABLE2) ||
		!dsCheckTable(table3, TYPE_C_S, nRow3, NROW_TABLE3) ||
		!dsCheckTable(table4, TYPE_D_S, nRow4, NROW_TABLE4) ||
		!checkTable5(pDataset)
	) {
		printf("readDynamic - dsCheckTable failed\n");
		goto fail;
	}
	printf("readDynamic - success\n");
	rtn = TRUE;
	goto done;
fail:
	rtn = FALSE;
	goto done;
done:
	if (pDataset != NULL) {
		dsFreeDataset(pDataset);
	}
	xdr_destroy(&xdr);
	fclose(stream);
	return rtn;
}
/******************************************************************************
*
* readVars - read dataset into program variables
*
*/
int readVars(void)
{
	TYPE_A_T table1[NROW_TABLE1], *pTable1 = table1;
	TYPE_B_T table2[NROW_TABLE2], *pTable2 = table2;
	TYPE_C_T table3[NROW_TABLE3], *pTable3 = table3;
	TYPE_D_T table4[NROW_TABLE4], *pTable4 = table4;
	size_t nRow1 = NROW_TABLE1, nRow2 = NROW_TABLE2;
	size_t nRow3 = NROW_TABLE3, nRow4 = NROW_TABLE4;
	DS_DATASET_T *pDataset;
	FILE *stream;
	XDR xdr;
	int rtn;
	
	/* open file and initialize xdr structure */
	if ((stream = fopen(SAMPLE_FILE, "rb")) == NULL) {
		printf("readVars: fopen failed\n");
		return FALSE;
	}
	xdrstdio_create(&xdr, stream, XDR_DECODE);

	/* read dataset into program vars */
	if (
		!xdr_dataset_type(&xdr, &pDataset) ||
		!dsMapTable(pDataset, "table1", TYPE_A_S, &nRow1, (char **)&pTable1) ||
		!dsMapTable(pDataset, "table2", TYPE_B_S, &nRow2, (char **)&pTable2) ||
		!dsMapTable(pDataset, "table3", TYPE_C_S, &nRow3, (char **)&pTable3) ||
		!dsMapTable(pDataset, "table4", TYPE_D_S, &nRow4, (char **)&pTable4) ||
		!dsAllocTables(pDataset) ||
		!xdr_dataset_data(&xdr, pDataset)
	) {
		dsPerror("readVars failed");
		goto fail;
	}
	/* check data */
	if (
		!dsCheckTable(table1, TYPE_A_S, nRow1, NROW_TABLE1) ||
		!dsCheckTable(table2, TYPE_B_S, nRow2, NROW_TABLE2) ||
		!dsCheckTable(table3, TYPE_C_S, nRow3, NROW_TABLE3) ||
		!dsCheckTable(table4, TYPE_D_S, nRow4, NROW_TABLE4) ||
		!checkTable5(pDataset)
	) {
		printf("readVars - dsCheckTable failed\n");	
		goto fail;
	}
	printf("readVars - success\n");
	rtn = TRUE;
	goto done;
fail:
	rtn = FALSE;
	goto done;
done:
	dsFreeDataset(pDataset);
	xdr_destroy(&xdr);
	fclose(stream);
	return rtn;

}
/******************************************************************************
*
* readProject - read into dynamic memory, project to program variables
*
*/
int readProject(void)
{
	TYPE_A_T table1[NROW_TABLE1], *pTable1 = table1;
	TYPE_B_T table2[NROW_TABLE2], *pTable2 = table2;
	TYPE_C_T table3[NROW_TABLE3], *pTable3 = table3;
	TYPE_D_T table4[NROW_TABLE4], *pTable4 = table4;
	size_t nRow1 = NROW_TABLE1, nRow2 = NROW_TABLE2;
	size_t nRow3 = NROW_TABLE3, nRow4 = NROW_TABLE4;
	DS_DATASET_T *pDataset = NULL;
	FILE *stream;
	XDR xdr;
	int rtn;
	
	/* open file and initialize xdr structure */
	if ((stream = fopen(SAMPLE_FILE, "rb")) == NULL) {
		printf("readProject: fopen failed\n");
		return FALSE;
	}
	xdrstdio_create(&xdr, stream, XDR_DECODE);

	/* read dataset into program vars */
	if (
		!xdr_dataset(&xdr, &pDataset) ||
		!dsTasProject(pDataset, "table1", TYPE_A_S, &nRow1, &pTable1) ||
		!dsTasProject(pDataset, "table2", TYPE_B_S, &nRow2, &pTable2) ||
		!dsTasProject(pDataset, "table3", TYPE_C_S, &nRow3, &pTable3) ||
		!dsTasProject(pDataset, "table4", TYPE_D_S, &nRow4, &pTable4)
	) {
		dsPerror("readProject failed");
		goto fail;
	}
	/* check data */
	if (
		!dsCheckTable(table1, TYPE_A_S, nRow1, NROW_TABLE1) ||
		!dsCheckTable(table2, TYPE_B_S, nRow2, NROW_TABLE2) ||
		!dsCheckTable(table3, TYPE_C_S, nRow3, NROW_TABLE3) ||
		!dsCheckTable(table4, TYPE_D_S, nRow4, NROW_TABLE4) ||
		!checkTable5(pDataset)
	) {	
		printf("readProject - dsCheckTable failed\n");
		goto fail;
	}
	nRow1 = NROW_TABLE1;
	if (!dsTasProject(pDataset, "table5", TYPE_A_S, &nRow1, &pTable1)) {
		dsPerror("result of project table5 into table1");
	}
	nRow2 = NROW_TABLE2;
	if (!dsTasProject(pDataset, "table5", TYPE_B_S, &nRow2, &pTable2)) {
		dsPerror("result of project table5 into table2");
	}
	printf("readProject - success\n");
	rtn = TRUE;
	goto done;
fail:
	rtn = FALSE;
	goto done;
done:
	dsFreeDataset(pDataset);
	xdr_destroy(&xdr);
	fclose(stream);
	return rtn;
}
/******************************************************************************
*
* writeDynamic - allocate memory and write dataset
*
*/
int writeDynamic(void)
{
	DS_DATASET_T *pDataset;
	FILE *stream;
	XDR xdr;
	int rtn;
	
	/* open file and initialize xdr structure */
	if ((stream = fopen(SAMPLE_FILE, "wb")) == NULL) {
		printf("writeDynamic: fopen failed\n");
		return FALSE;
	}
	xdrstdio_create(&xdr, stream, XDR_ENCODE);
	if (
		!dsNewDataset(&pDataset, "example") ||
		!dsAddTable(pDataset, "table1", TYPE_A_S, NROW_TABLE1, NULL) ||
		!dsAddTable(pDataset, "table2", TYPE_B_S, NROW_TABLE2, NULL) ||
		!dsAddTable(pDataset, "table3", TYPE_C_S, NROW_TABLE3, NULL) ||
		!dsAddTable(pDataset, "table4", TYPE_D_S, NROW_TABLE4, NULL) ||
		!dsAddTable(pDataset, "table5", TYPE_B_S, NROW_TABLE5, NULL) ||
		!dsSetDataset(pDataset)
	) {
		dsPerror("writeDynamic setup failed");
		goto fail;
	}
	/* write tables */
	if (!xdr_dataset(&xdr, &pDataset)) {
		dsPerror("writeDynamic xdr_dataset failed");
		goto fail;
	}
	printf("writeDynamic - success\n");
	rtn = TRUE;
	goto done;
fail:
	rtn = FALSE;
	goto done;
done:
	if (pDataset != NULL) {
		dsFreeDataset(pDataset);
	}
	xdr_destroy(&xdr);
	fclose(stream);
	return rtn;
}
/******************************************************************************
*
* writeVars - write dataset from program variables
*
*/
int writeVars(void)
{
	TYPE_A_T table1[NROW_TABLE1];
	TYPE_B_T table2[NROW_TABLE2];
	TYPE_C_T table3[NROW_TABLE3];
	TYPE_D_T table4[NROW_TABLE4];
	TYPE_A_T table5[NROW_TABLE5];
	char *pTable1 = (char *)table1, *pTable2 = (char *)table2;
	char *pTable3 = (char *)table3, *pTable4 = (char *)table4;
	char *pTable5 = (char *)table5;
	size_t nRow1 = NROW_TABLE1, nRow2 = NROW_TABLE2;
	size_t nRow3 = NROW_TABLE3, nRow4 = NROW_TABLE4;
	size_t nRow5 = NROW_TABLE5;
	DS_DATASET_T *pDataset;
	FILE *stream;
	XDR xdr;
	int rtn;
	
	/* open file and initialize xdr structure */
	if ((stream = fopen(SAMPLE_FILE, "wb")) == NULL) {
		printf("writeVars: fopen failed\n");
		return FALSE;
	}
	xdrstdio_create(&xdr, stream, XDR_ENCODE);

	/* build and write dataset */
	if (
		!dsNewDataset(&pDataset, "example") ||
		!dsAddTable(pDataset, "table1", TYPE_A_S, nRow1, &pTable1) ||
		!dsAddTable(pDataset, "table2", TYPE_B_S, nRow2, &pTable2) ||
		!dsAddTable(pDataset, "table3", TYPE_C_S, nRow3, &pTable3) ||
		!dsAddTable(pDataset, "table4", TYPE_D_S, nRow4, &pTable4) ||
		!dsAddTable(pDataset, "table5", TYPE_A_S, nRow5, &pTable5) ||
		!dsSetDataset(pDataset) ||
		!xdr_dataset(&xdr, &pDataset)
	) {
		dsPerror("writeVars failed");
		goto fail;
	}
	printf("writeVars - success\n");
	rtn = TRUE;
	goto done;
fail:
	rtn = FALSE;
	goto done;
done:
	xdr_destroy(&xdr);
	fclose(stream);
	dsFreeDataset(pDataset);
	return rtn;
}
#endif /* WIN32 */
