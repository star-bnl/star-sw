/* Copyright 1993, Lawrence Berkeley Laboratory */

/* checklib.c - data structure test */

/*
modification history
--------------------
23jul93,whg  written.
11jun96,whg  added indirection to dataset structure
*/

/*
DESCRIPTION
routines to verify data read by sample program
*/
#include <stddef.h>
#define DS_PRIVATE
#include "dsxdr.h"
/******************************************************************************
*
*/
#define DATA_DIM 13
typedef struct test_type_t {
	char c;
	double d;
	float f;
	long l;
	octet o;
	short s;
	unsigned long ul;
	unsigned short us;
	struct point_t {
		char flag;
		float x, y, z;
		octet data[DATA_DIM];
	}point;
}TEST_TYPE_T;

static char *testSpecifier = "struct test_type_t {"
	"char c;"
	"double d;"
	"float f;"
	"long l;"
	"octet o;"
	"short s;"
	"unsigned long ul;"
	"unsigned short us;"
	"struct point_t {"
		"char flag;"
		"float x, y, z;"
		"octet data[13];"
	"}point;}";

static char *emptySpecifier = "struct empty{char c;}";

static int dsCheckTableR(void *pData, size_t count, DS_TYPE_T *type);
static int dsCheckTest(TEST_TYPE_T *pTest, size_t count);
static int dsSetTableR(void *pData, size_t count, DS_TYPE_T *type);
/******************************************************************************
*
* dsCheckDataset - check that all tables have basic type code in each element
*
* RETURN: TRUE if OK FALSE if error
*/
int dsCheckDataset(DS_DATASET_T *pDataset)
{
	size_t i;
	DS_TYPE_T *type;
	DS_LIST_T list;

	if(!dsListInit(&list)) {
		return FALSE;
	}
	if (!dsVisitList(&list, pDataset)) {
		goto fail;
	}
	for (i = 0; i < list.count; i++) {
		pDataset = list.pItem[i];
		if (DS_IS_TABLE(pDataset)) {
			if (!dsTypePtr(&type, pDataset->tid) ||
				!dsCheckTableR(pDataset->p.data, pDataset->elcount, type)) {
				goto fail;
			}
		}
	}
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
}
/******************************************************************************
*
* dsCheckTable - verify that elements of a table contain their type code
*
* RETURN: TRUE if OK FALSE if error
*/
int dsCheckTable(void *pData, char *decl, size_t nRow, size_t expectedNRow)
{
	size_t tid;
	DS_TYPE_T *type;

	if (nRow != expectedNRow) {
		printf("dsCheckTable: nRow not correct\n");
		return FALSE;
	}
	if (!dsTypeId(&tid, decl, NULL) || !dsTypePtr(&type, tid)) {
		dsPerror("dsCheckTable: invalid decl");
		return FALSE;
	}
	return dsCheckTableR(pData, nRow, type);
}
/******************************************************************************
*
* dsCheckTableR - recursive part of dsCheckTable
*
* RETURN: TRUE if OK FALSE if error
*/
static int dsCheckTableR(void *pData, size_t count, DS_TYPE_T *type)
{
	size_t code, i, j;
	DS_FIELD_T *field;
	DS_PTR_UNION_T p;

	p.v = pData;
	switch(code = type->code) {
	case DS_TYPE_CHAR:
	case DS_TYPE_OCTET:
		for (i= 0; i < count && p.c[i] == (char)code; i++);
		break;

	case DS_TYPE_SHORT:
	case DS_TYPE_U_SHORT:
		for (i= 0; i < count && p.s[i] == (short)code; i++);
		break;

	case DS_TYPE_LONG:
	case DS_TYPE_U_LONG:
		for (i= 0; i < count && p.l[i] == (long)code; i++);
		break;
	
	case DS_TYPE_FLOAT:
		for (i= 0; i < count && p.f[i] == code; i++);
		break;
	
	case DS_TYPE_DOUBLE:
		for (i= 0; i < count && p.d[i] == code; i++);
		break;

	case DS_TYPE_STRUCT:
		for (i = 0; i < count; i++) {
			field = DS_FIELD_PTR(type);
			for (j = type->nField; j > 0; field++, j--) {
				if (!dsCheckTableR(p.c + field->offset,
					field->count, field->type)) {
					break;
				}
			}
			if (j > 0) {
				break;
			}
			p.c += type->size;
		}
		break;
	default:
		printf("dsCheckTable: invalid type->code\n");
		return FALSE;
	}
	if (i == count) {
		return TRUE;
	}
	printf("dsCheckTable: invalid type - %s\n", type->name);
	return FALSE;
}
/******************************************************************************
*
* dsCheckTest - check that alignment and values are correct
*
* RETURN: TRUE if OK FALSE if error
*/
static int dsCheckTest(TEST_TYPE_T *pTest, size_t count)
{
	size_t i, j;

	for (i = 0; i < count; i++) {
		if (
			pTest->c != DS_TYPE_CHAR    ||
			pTest->d != DS_TYPE_DOUBLE  ||
			pTest->f != DS_TYPE_FLOAT   ||
			pTest->l != DS_TYPE_LONG    ||
			pTest->o != DS_TYPE_OCTET   ||
			pTest->s != DS_TYPE_SHORT   ||
			pTest->ul != DS_TYPE_U_LONG ||
			pTest->us != DS_TYPE_U_SHORT) {
			dsErrorPrint("dsCheckTest: failed for basic types\n");
			return FALSE;
		}
		for (j = 0; j < DATA_DIM; j++) {
			if (pTest->point.data[j] != DS_TYPE_OCTET) {
				break;
			}
		}
		if (j != DATA_DIM ||
			pTest->point.flag != DS_TYPE_CHAR ||
			pTest->point.x != DS_TYPE_FLOAT   ||
			pTest->point.y != DS_TYPE_FLOAT   ||
			pTest->point.z != DS_TYPE_FLOAT) {
			dsErrorPrint("dsCheckTest: failed for complex types\n");
			return FALSE;
		}
	}
	return TRUE;
}	
/******************************************************************************
*
* dsReadAll - read datasets
*
* RETURN: TRUE for success else FALSE
*/
int dsReadAll(XDR *xdrs)
{
	DS_DATASET_T *pDataset;
	size_t i, *tList = NULL;

	if (!dsTypeListCreate(&tList, DS_XDR_HASH_LEN + 1)) {
		dsPerror("dsReadAll: dsTypeListCreate failed"); 
		return FALSE;
	}
	for (i = 0;; i++) {
		pDataset = NULL;
		if (!xdr_dataset(xdrs, &pDataset)) {
			printf("dsReadAll read %d datasets\n", i);
			dsPerror("dsReadAll xdr_dataset status");
			goto fail;
		}
		if (!dsPrintTypes(stdout, pDataset, tList) ||
			!dsPrintDatasetSpecifier(stdout, pDataset)) {
			dsPerror("dsReadAll: print failed");
			goto fail;
		}
		dsFreeDataset(pDataset);
	}
fail:
	dsFreeDataset(pDataset);
	dsTypeListFree(tList);
	return FALSE;
}
/******************************************************************************
*
* dsReadTest - read datasets and check values
*
* RETURN: TRUE for success else FALSE
*/
int dsReadTest(XDR *xdrs, size_t count)
{
	DS_DATASET_T *pDataset, *pTable;
	size_t i;

	for (i = 0; i < count; i++) {
		pDataset = NULL;
		if (!xdr_dataset(xdrs, &pDataset) ||
			!dsFindTable(&pTable, pDataset, "empty", emptySpecifier) ||
			pTable->elcount != 0 ||
			!dsFindTable(&pTable, pDataset, "table", testSpecifier) ||
			pTable->elcount != i ||
			!dsCheckTest(pTable->p.data, i)
		){
			dsPerror("dsReadTest Failed");
			if (pDataset != NULL) {
				dsFreeDataset(pDataset);
			}
			return FALSE;
		}
		dsFreeDataset(pDataset);
	}
	return TRUE;
}/******************************************************************************
*
* dsSetDataset - set all tables to have basic type code in each element
*
* RETURN: TRUE for success else FALSE
*/
int dsSetDataset(DS_DATASET_T *pDataset)
{

	size_t i;
	DS_TYPE_T *type;
	DS_LIST_T list;

	if(!dsListInit(&list)) {
		return FALSE;
	}
	if (!dsVisitList(&list, pDataset)) {
		goto fail;
	}
	for (i = 0; i < list.count; i++) {
		pDataset = list.pItem[i];
		if (DS_IS_TABLE(pDataset)) {
			if (!dsTypePtr(&type, pDataset->tid) ||
				!dsSetTableR(pDataset->p.data, pDataset->maxcount, type)) {
				goto fail;
			}
			pDataset->elcount = pDataset->maxcount;
		}
	}
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
}
/******************************************************************************
*
* dsSetTableR - recursive part of dsSetTable
*
* RETURN: TRUE for success else FALSE
*/
static int dsSetTableR(void *pData, size_t count, DS_TYPE_T *type)
{
	size_t code, i, j;
	DS_FIELD_T *field;
	DS_PTR_UNION_T p;

	p.v = pData;

	switch(code = type->code) {
	case DS_TYPE_CHAR:
	case DS_TYPE_OCTET:
		for (i = 0; i < count; i++) {
			p.c[i] = (char)code;
		}
		break;

	case DS_TYPE_SHORT:
	case DS_TYPE_U_SHORT:
		for (i = 0; i < count; i++) {
			p.s[i] = (short)code;
		}
		break;

	case DS_TYPE_LONG:
	case DS_TYPE_U_LONG:
		for (i = 0; i < count; i++) {
			p.l[i] = code;
		}
		break;
	
	case DS_TYPE_FLOAT:
		for (i = 0; i < count; i++) {
			p.f[i] = (float)code;
		}
		break;
	
	case DS_TYPE_DOUBLE:
		for (i = 0; i < count; i++) {
			p.d[i] = code;
		}
		break;

	case DS_TYPE_STRUCT:
		for (i = 0; i < count; i++) {
			field = DS_FIELD_PTR(type);
			for (j = type->nField; j-- > 0; field++) {
				if (!dsSetTableR(p.c + field->offset,
					field->count, field->type)) {
					return FALSE;
				}
			}
			p.c += type->size;
		}
		break;
	default:
		printf("dsSetTable: invalid type->code %d\n", code);
		return FALSE;
	}
	return TRUE;
}	
/******************************************************************************
*
* dsWriteTest - write set of test datasets
*
* RETURN: TRUE for success else FALSE
*/
int dsWriteTest(XDR *xdrs, size_t count)
{
	DS_DATASET_T *cycle, *pDataset, *pTable = NULL;
	TEST_TYPE_T *pData = NULL;
	size_t i;

	if (
		!dsNewDataset(&pDataset, "test") ||
		!dsNewDataset(&cycle, "cycle") ||
		!dsLink(pDataset, cycle) ||
		!dsLink(cycle, pDataset) ||
		!dsAddTable(pDataset, "empty", emptySpecifier, 0, NULL) ||
		!dsAddTable(pDataset, "table", testSpecifier, count, (char **)&pData) ||
		!dsFindTable(&pTable, pDataset, "table", testSpecifier) || 
		!dsSetDataset(pDataset) ||
		pTable->elcount != count || pTable->maxcount != count
	) {
		if (pTable != NULL) {
			printf("count %d, elcount %d, maxcount %d\n", 
				count, pTable->elcount, pTable->maxcount);
		}
		dsPerror("dsWriteTest setup failed");
		goto fail;
	}
	if (!dsCheckTest(pData, count)) {
		goto fail;
	}
	/* write tables */
	for (i = 0; i < count; i++) {
		pTable->elcount = i;
		if (!xdr_dataset(xdrs, &pDataset)) {
			dsPerror("writeDynamic xdr_dataset failed");
			goto fail;
		}
	}
	if (pDataset != NULL) {
		dsFreeDataset(pDataset);
	}
	return TRUE;
fail:
	if (pDataset != NULL) {
		dsFreeDataset(pDataset);
	}
	return FALSE;
}
	
