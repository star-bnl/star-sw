/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dsprint.c - formated I/O for tables and datasets */
/*
modification history
--------------------
15mar95,whg  written.
11jun96,whg  added indirection to dataset structure
*/

/*
DESCRIPTION
TBS ...
*/
#include <string.h> 
#define DS_PRIVATE
#include "dsxdr.h"

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
			/* 04sep97-cet- BUG FIX
			fprintf(stream, "\t%s", p.c);*/
			fprintf( stream,  "\t");
			for (i = 0; i < count; i++) {
				fprintf( stream,  "%c", p.c[i]);
			}
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
	DS_BUF_T bp;

	DS_PUT_INIT(&bp, buf, sizeof(buf));
	if (!dsDatasetSpecifier(&bp, pDataset)) {
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
int dsPrintTypes(FILE *stream, DS_DATASET_T *dataset, size_t *tList)
{
	const char *str;
	size_t h, i, len;
	DS_DATASET_T *item;
	DS_LIST_T list;
	DS_TYPE_T *pType;

	if (!dsListInit(&list)) {
		return FALSE;
	}
	if (!dsVisitList(&list, dataset)) {
		goto fail;
	}
	for (i = 0; i < list.count; i++) {
		item = list.pItem[i];
		if (!DS_IS_TABLE(item)) {
			continue;
		}
		if (!dsTypePtr(&pType, item->tid) ||
			!dsTypeListFind(&h, tList, pType->name)) {
			goto fail;
		}
		if (tList[h] == item->tid) {
			continue;
		}
		if (tList[h]) {
			DS_LOG_ERROR(DS_E_DUPLICATE_TYPE_NAME);
			goto fail;
		}
		tList[h] = item->tid;
		if (!dsTypeSpecifier(&str, &len, item->tid)) {
			goto fail;
		}
		fprintf(stream, "type %s\n", str);
	}
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
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
	const char *str;
	size_t i;
	
	if (!dsTypeSpecifier(&str, &i, pTable->tid)) {
		fprintf(stream, "\nINVALID TABLE\n\n");
		return;
	}
	fprintf(stream, "%s %s[%d]\n", str, pTable->name, pTable->elcount);
	return;
}

	
