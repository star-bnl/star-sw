/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dsprint.c - formated I/O for tables and datasets */
/*
modification history
--------------------
15mar95,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <string.h> 
#define DS_PRIVATE
#include "dscodes.h"
#include "dsxdr.h"

/****************************************************************************
*/
void testPrint()
{
	char *spec = 
	"struct test {char x[10]; octet o; short s; unsigned short us;"
		"long l; unsigned long ul; float f; double d;}";
	struct test {char x[10]; octet o; short s; unsigned short us;
		long l; unsigned long ul; float f; double d;}
	data = {"hi", 129, 2, 6000, 123, 300000000, 1.23f, 4.46};
	
	DS_DATASET_T table, *pTable = &table;
	if (!dsNewTable(&pTable, "table", spec, 1, &data)) {
		dsPerror("dsNewTable failed for testPrint ");
		return;
	}
	dsPrintTableData(stdout, pTable);
	
}	
/*****************************************************************************
*
* dsPrintData - print data field
*
*/
void dsPrintData(FILE *stream, DS_TYPE_T *type, unsigned count, void *data)
{
	size_t i;
	DS_PTR_UNION_T p;
	DS_FIELD_T *field, *limit;
	
	p.v = data;
	
	switch(type->code) {

		case DS_TYPE_CHAR:
				fprintf(stream, "\t%s", p.c);
			break;
			
		case DS_TYPE_OCTET:
			for (i = 0; i < count; i++) {
				fprintf(stream, "\t%u", p.o[i]);
			}
			break;
			
		case DS_TYPE_SHORT:
			for (i = 0; i < count; i++) {
				fprintf(stream, "\t%hd", p.s[i]);
			}
			break;
			
		case DS_TYPE_U_SHORT:
			for (i = 0; i < count; i++) {
				fprintf(stream, "\t%hu", p.us[i]);
			}
			break;
			
		case DS_TYPE_LONG:
			for (i = 0; i < count; i++) {
				fprintf(stream, "\t%ld", p.l[i]);
			}
			break;
			
		case DS_TYPE_U_LONG:
			for (i = 0; i < count; i++) {
				fprintf(stream, "\t%lu", p.ul[i]);
			}
			break;
			
		case DS_TYPE_FLOAT:
			for (i = 0; i < count; i++) {
				fprintf(stream, "\t%g", p.f[i]);
			}
			break;
			
		case DS_TYPE_DOUBLE:
			for (i = 0; i < count; i++) {
				fprintf(stream, "\t%g", p.d[i]);
			}
			break;
			
		case DS_TYPE_STRUCT:
			limit = DS_FIELD_PTR(type) + type->nField;
			for (i = 0; i < count; i++) {
				for (field = DS_FIELD_PTR(type); field < limit; field++) {
					dsPrintData(stream, field->type, field->count,
						p.c + field->offset);
				}
				p.c += type->size;
			}
			break;
			
		default:
			fprintf(stream, "\tINVALID_TYPE");
			break;
	}
}
/*****************************************************************************
*
* dsPrintDatasetSpecifier - print declaration for a dataset
*
* RETURNS: TRUE if success else FALSE
*/
int dsPrintDatasetSpecifier(FILE *stream, DS_DATASET_T *pDataset)
{
	char buf[DS_MAX_SPEC_LEN+1];

	if (!dsDatasetSpecifier(buf, sizeof(buf), pDataset)) {
		return FALSE;
	}
	fprintf(stream, "%s\n", buf);
	return TRUE;
}
/******************************************************************************
*
* dsPrintSpecifiers - print dataset specifiers
*
* RETURN: TRUE if success else FALSE
*/
int dsPrintSpecifiers(FILE *stream, DS_DATASET_T *pDataset)
{
	size_t *tList = NULL;

	if (!dsTypeListCreate(&tList, DS_XDR_HASH_LEN + 1)) {
		return FALSE;
	}
	if (!dsPrintTypes(stream, pDataset, tList) ||
		!dsPrintDatasetSpecifier(stream, pDataset)) {
		goto fail;
	}
	dsTypeListFree(tList);
	return TRUE;
fail:
	dsTypeListFree(tList);
	return FALSE;
}
/******************************************************************************
*/
int dsPrintTypes(FILE *stream, DS_DATASET_T *pDataset, size_t *tList)
{
	char *str;
	size_t h, i;
	DS_TYPE_T *pType;

	if (DS_IS_TABLE(pDataset)) {
		if (!dsTypePtr(&pType, pDataset->tid)) {
			return FALSE;
		}
		if (!dsTypeListFind(&h, tList, pType->name)) {
			return FALSE;
		}
		if (tList[h] == pDataset->tid) {
			return TRUE;
		}
		if (tList[h]) {
			DS_ERROR(DS_E_DUPLICATE_TYPE_NAME);
		}
		if (!dsTypeSpecifier(&str, &i, pDataset->tid)) {
			return FALSE;
		}
		fprintf(stream, "%s\n", str);
		tList[h] = pDataset->tid;
		return TRUE;
	}
	if (DS_IS_DATASET(pDataset)) {
		for (i = 0; i < pDataset->elcount; i++) {
			if (!dsPrintTypes(stream, &pDataset->p.child[i], tList)) {
				return FALSE;
			}
		}
		return TRUE;
	}
	DS_ERROR(DS_E_SYSTEM_ERROR);
}
/****************************************************************************
*
* dsPrintTable - print table data
*
*/
void dsPrintTableData(FILE *stream, DS_DATASET_T *table)
{
	size_t i;
	DS_TYPE_T *type;	

	if (!dsTypePtr(&type, table->tid)) {
		fprintf(stream, "\nINVALID TABLE\n\n");
		return;
	}

	for (i = 0; i < table->elcount; i++) {
		dsPrintData(stream, type, 1, (char *)table->p.data + i*type->size);
		fprintf(stream, "\n");
	}
}
/****************************************************************************
*
* dsPrintTableType - print table type specifier
*
*/
void dsPrintTableType(FILE *stream, DS_DATASET_T *pTable)
{
	char *str;
	size_t i;
	
	if (!dsTypeSpecifier(&str, &i, pTable->tid)) {
		fprintf(stream, "\nINVALID TABLE\n\n");
		return;
	}
	fprintf(stream, "%s %s[%d]\n", str, pTable->name, pTable->elcount);
	return;
}

	
