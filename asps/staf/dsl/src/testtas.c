/* Copyright 1994, Lawrence Berkeley Laboratory */

/* testtas.c - test prototype tas to ds interface */

/*
modification history
--------------------
01a,13apr94,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include "tasfake.h"
#include "dscodes.h"
#include <rpc/rpc.h>
#include "dstype.h"
#include "dsxdr.h"
#ifdef sun
int printf(char *fmt, ...);
int fprintf(FILE *stream, char *fmt, ...);
char *memset(void *ptr, int val , int n);
void fclose(FILE *stream);
#endif
typedef struct both_type {float x, y, z;}BOTH_TYPE;
#define BOTH_DECL "struct both_type {float x, y, z;}"
#define BOTH_DIM 20

typedef struct src_type {float a, b, c, e, g;}SRC_TYPE;
#define SRC_DECL "struct src_type {float a, b, c, e, g;};"
#define SRC_DIM 30

typedef struct dst_type {float a, c, e, f;}DST_TYPE;
#define DST_DECL "struct dst_type {float a, c, e, f;};"
#define DST_DIM 40

typedef struct misc_type {float u, v, w;}MISC_TYPE;
#define MISC_DECL "struct dst_type {float u, v, w;};"
#define MISC_DIM 50

#define DSET_DIM 10

int tasDataset(XDR *xdrs, DS_DATASET_T *pDset);
int readFile(char *fileName);
int tasNOK(DS_DATASET_T *pDset, int set);
int writeFile(char *fileName);
int xdrCreate(XDR *xdrs, char *fileName, int op);
void xdrstdio_create(XDR *xdrs, FILE *stream, int op);
/******************************************************************************
*/
void main()
{
	char *fileName = "tas.bin";

	if (!writeFile(fileName) || !readFile(fileName)){
		dsPerror("fail");
	}
}
/******************************************************************************
*/
int readFile(char *fileName)
{
	size_t i;
	BOTH_TYPE both[BOTH_DIM], *pBoth = both;
	MISC_TYPE notThere[MISC_DIM], *pNotThere = notThere;	
	DST_TYPE dstVar[DST_DIM], *pDstVar = dstVar;
	DS_DATASET_T dstDset[DSET_DIM], *pDstDset = dstDset;
	TAS_FAKE_T tasBoth, tasNot, tasDst;
	XDR xdr;

	memset(dstVar, 0, sizeof(dstVar));
	if (
		!dsNewDataset(&pDstDset, "data", DSET_DIM) ||
		!dsAddTable(pDstDset, "both", BOTH_DECL, BOTH_DIM, &pBoth, &tasBoth) ||
		!dsAddTable(pDstDset, "notThere", MISC_DECL, MISC_DIM, &pNotThere, &tasNot) ||
		!dsAddTable(pDstDset, "table1", DST_DECL, DST_DIM, &pDstVar, &tasDst)
	){
		dsPerror("readFile create dataset failed");
		return FALSE;
	}
	if (!xdrCreate(&xdr, fileName, XDR_DECODE) ||
		!tasDataset(&xdr, pDstDset)
	){
		dsPerror("readFile: xdr failed\n");
		return FALSE;
	}
	/* copy elcount from pDstDset to nok */
	tasNOK(pDstDset, TRUE);

	printf("tasBoth.nok %d, tasNot.nok %d, tasDst.nok %d\n",
		tasBoth.nok, tasNot.nok, tasDst.nok);
	printf("\ntasDst:\n"); 
	for (i = 0; i < tasDst.nok; i++) {
		printf("%d\t%g\t%g\t%g\t%g\n", i, dstVar[i].a,
			dstVar[i].c, dstVar[i].e, dstVar[i].f);
	}
	printf("\nboth:\n");
	for (i = 0; i < tasBoth.nok; i++) {
		printf("%i\t%g\t%g\t%g\n", i, both[i].x, both[i].y, both[i].z);
	}
	return TRUE;
}
/******************************************************************************
*/
int tasNOK(DS_DATASET_T *pDset, int set)
{
	size_t i;
	TAS_FAKE_T *pTas;

	if (DS_IS_TABLE(pDset)) {
		if ((pTas = (TAS_FAKE_T *)pDset->pUser) != NULL) {
			if (set) {
				pTas->nok = pDset->elcount;
			}
			else {
				pDset->elcount = pTas->nok;
			}
		}
	}
	else {
		for (i = 0; i < pDset->elcount; i++) {
			tasNOK(&pDset->p.child[i], set);
		}
	}
	return TRUE;
}
/******************************************************************************
*/
int writeFile(char *fileName)
{
	size_t i;
	BOTH_TYPE both[BOTH_DIM], *pBoth = both;
	MISC_TYPE extra[MISC_DIM], *pExtra = extra;
	SRC_TYPE srcVar[SRC_DIM], *pSrcVar = srcVar;
	DS_DATASET_T srcDset[DSET_DIM], *pSrcDset = srcDset;
	TAS_FAKE_T tasBoth, tasExtra, tasSrc;
	XDR xdr;

	tasBoth.nok = 5;
	tasExtra.nok = 10;
	tasSrc.nok = 15;

	for (i = 0; i < tasSrc.nok; i++) {
		srcVar[i].a = (float)(10*i + 1);
		srcVar[i].b = (float)(10*i + 2);
		srcVar[i].c = (float)(10*i + 3);
		srcVar[i].e = (float)(10*i + 4);
		srcVar[i].g = (float)(10*i + 5);
	}
	for (i = 0; i < tasBoth.nok; i++) {
		both[i].x = (float)(100*i +10);
		both[i].y = (float)(100*i +20);
		both[i].z = (float)(100*i +30);
	}
	if (
		!dsNewDataset(&pSrcDset, "data", DSET_DIM) ||
		!dsAddTable(pSrcDset, "both", BOTH_DECL, BOTH_DIM, &pBoth, &tasBoth) ||
		!dsAddTable(pSrcDset, "extra", MISC_DECL, MISC_DIM, &pExtra, &tasExtra) ||
		!dsAddTable(pSrcDset, "table1", SRC_DECL, SRC_DIM, &pSrcVar, &tasSrc)
	){
		dsPerror("writeFile create dataset failed");
		return FALSE;
	}
	/* copy nok to pSrcDset elcount for each table */
	tasNOK(pSrcDset, FALSE);

	if (!xdrCreate(&xdr, fileName, XDR_ENCODE) ||
		!tasDataset(&xdr, pSrcDset)
	){
		dsPerror("writeFile: xdr failed\n");
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
		fprintf(stderr, "fopen(%s, %s) failed\n", fileName, type);
		return FALSE;
	}
	xdrstdio_create(xdrs, stream, op);
	return TRUE;
}
