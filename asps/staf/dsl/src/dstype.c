/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dstype.c - routine to define data structure type */

/*
modification history
--------------------
01a,24apr93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#include "dscodes.h"
#include "dstype.h"

static int dsBasicInit(void);
static DS_TYPE_T *dsFindTag(char *tag, DS_MEM_T *mem);
static int dsCreateTypeR(DS_TYPE_T **pType,
	char *str, char **ptr, DS_MEM_T *mem);
static int dsFmtTypeDefR(DS_MEM_T *bp, DS_TYPE_T *type, int level,
	size_t *pNTag, DS_TYPE_T **tag);

/******************************************************************************
*
* basicType - definition of basic types
*
*/
#define INIT_BASIC(n, c, t, s) {if (0 >= nt--) goto fail;strcpy(type->name, n);\
	type->code = c; type->flags = sizeof(t) == (s) ? repFlag : 0;\
	type->size = sizeof(t); type->modulus = DS_MODULUS(t);\
	type->stdsize = s; type->stdmodulus = s; type++;}

static DS_TYPE_T basicType[DS_TYPE_COUNT];
static size_t nBasic = 0;
/******************************************************************************
*
* dsBasicInit - initialize basic types
*
*/
static int dsBasicInit()
{
	int nt = DS_TYPE_COUNT;
	unsigned repFlag = DS_STD_REP;
	DS_TYPE_T *type = basicType;

	/********** start critical section *******************************/
	if (!dsTypeLock()) {
		return FALSE;
	}
	if (nBasic != 0) {
		return dsTypeUnlock();
	}
	memset((char *)basicType, 0, sizeof(basicType));

	INIT_BASIC("char",	DS_TYPE_CHAR,		char,		1);
	INIT_BASIC("byte",	DS_TYPE_BYTE,		signed char,	1);
	INIT_BASIC("u_byte",	DS_TYPE_U_BYTE,		unsigned char,	1);

	if (!DS_IS_BIG_ENDIAN) {
		repFlag = 0;
	}
	INIT_BASIC("short",	DS_TYPE_SHORT,		short,		2);
	INIT_BASIC("u_short",	DS_TYPE_U_SHORT,	unsigned short,	2);
	INIT_BASIC("int",	DS_TYPE_INT,		int,		4);
	INIT_BASIC("u_int",	DS_TYPE_U_INT,		unsigned int,	4);
	INIT_BASIC("long",	DS_TYPE_LONG,		long,		4);
	INIT_BASIC("u_long",	DS_TYPE_U_LONG,		unsigned long,	4);

	if (!DS_IS_IEEE_FLOAT) {
		repFlag = 0;
	}
	INIT_BASIC("float",	DS_TYPE_FLOAT,		float,		4);
	INIT_BASIC("double",	DS_TYPE_DOUBLE,		double,		8);

	nBasic = type - basicType;
	return dsTypeUnlock();

fail:
	if (!dsTypeUnlock()) {
		return FALSE;
	}
	/********** end critical section *******************************/
	DS_ERROR(DS_E_SYSTEM_ERROR);
}
/******************************************************************************************
*
* dsCmpFieldType - compare two fields for project compatibility
*
*/
int dsCmpFieldType(DS_FIELD_T *f1, DS_FIELD_T *f2)
{
	int i;

	if (f1->count != f2->count || dsTypeCmp(f1->type, f2->type) != 0) {
		return 1;
	}
	for (i = 0; i < DS_MAX_DIMS; i++) {
		if (f1->dim[i] != f2->dim[i]) {
			return 1;
		}
		if (f1->dim[i] == 0) {
			break;
		}
	}
	return 0; 
}
/******************************************************************************
*
* dsCreateType - create a type structure for a type declaration string
*
*/
int dsCreateType(DS_TYPE_T **pType, size_t *pSize, char *str, char **ptr)
{
	int map[DS_MAX_STRUCT], nMap, nSep;
	size_t size;
	DS_MEM_T mem;
	DS_TYPE_T *type;

	if (nBasic == 0  && !dsBasicInit()) {
		return FALSE;
	}
	if (!dsMapDef(&nSep, map, &nMap, DS_MAX_STRUCT, str)) {
		return FALSE;
	}
	if (nMap == 0) {
		DS_ERROR(DS_E_INVALID_TYPE);
	}
	size = nMap*sizeof(DS_TYPE_T) + nSep*sizeof(DS_FIELD_T);
	if ((mem.first = (char *)*pType) != NULL) {
		if (*pSize < size) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}
		memset(mem.first, 0 , size);
	}
	else if ((mem.first = dsTypeCalloc(size)) == NULL) {
		return FALSE;
	}
	mem.next = mem.first;
	mem.limit = mem.first + size;
	mem.map = map;
	if (!dsCreateTypeR(&type, str, ptr, &mem)) {
		goto fail;
	}
	if (mem.limit != mem.next) {
		DS_LOG_ERROR(DS_E_SYSTEM_ERROR);
		goto fail;
	}
	if (pSize != NULL) {
		*pSize = size;
	}
	*pType = type;
	return TRUE;
fail:
	if (*pType == NULL) {
		dsTidFree(mem.first, size);
	}
	return FALSE;
}
/******************************************************************************
*
* dsCreateTypeR - second pass for parse of type declaration
*
*/
static int dsCreateTypeR(DS_TYPE_T **pType, char *str, char **ptr, DS_MEM_T *mem)
{
	char tag[DS_NAME_DIM], *tst;
	int j, n, nField;
	size_t i;
	DS_FIELD_T *field;
	DS_TYPE_T *fieldType, *type;

	for (i = 0; i < nBasic; i++) {
		if (dsNextName(str, ptr, basicType[i].name)) {
			*pType = &basicType[i];
			return TRUE;
		}
	}

	if (!dsNextName(str, &str, "struct")) {
		DS_ERROR(DS_E_INVALID_TYPE_NAME);
	}

	if(dsIsName(str)) {
		if (!dsIdentifier(tag, str, &str)) {
			return FALSE;
		}
		if ((type = dsFindTag(tag, mem)) != NULL) {
			if (ptr != NULL) {
				*ptr = str;
			}
			*pType = type;
			return TRUE;
		}
	}
	else {
		tag[0] = '\0';
	}

	if (!dsNextChar(str, &str, '{')) {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	type = (DS_TYPE_T *)mem->next;
	field = (DS_FIELD_T *)&type[1];
	nField = *mem->map++;
	if (nField == 0) {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	mem->next = (char *)&field[nField];
	if (mem->next > mem->limit) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	type->code = DS_TYPE_STRUCT;
	type->flags = DS_STD_REP;
	type->nField = nField;
	type->field = field;
	for(i = type->nField, fieldType = NULL; i-- > 0 ; field++) {
		if (fieldType == NULL) {
			if (!dsCreateTypeR(&fieldType, str, &str, mem)) {
				return FALSE;
			}
			type->size += DS_PAD(type->size, fieldType->modulus);

			if (fieldType->modulus > type->modulus) {
				type->modulus = fieldType->modulus;
			}
			type->stdsize += DS_PAD(type->stdsize,
						fieldType->stdmodulus);

			if (fieldType->stdmodulus > type->stdmodulus) {
				type->stdmodulus = fieldType->stdmodulus;
			}
			if (!DS_REP_IS_STD(fieldType) ||
				type->size != type->stdsize) {
				type->flags &= ~DS_STD_REP;
			}
		}
		if (!dsIdentifier(field->name, str, &str)) {
			return FALSE;
		}
		field->type = fieldType;
		field->count = 1;
		for (j = 0; dsNextChar(str, &str, '['); j++) {
			if (j == DS_MAX_DIMS) {
				DS_ERROR(DS_E_TOO_MANY_DIMENSIONS);
			}
			n = (int)strtol(str, &tst, 10);
			if (str == tst || n <= 0 ||
				!dsNextChar(tst, &str, ']')) {
				DS_ERROR(DS_E_INVALID_DIMENSION);
			}
			field->dim[j] = n;
			field->count *= n;
		}
		field->offset = type->size;
		field->stdoffset = type->stdsize;
		type->size += field->count*fieldType->size;
		type->stdsize += field->count*fieldType->stdsize;

		if (dsNextChar(str, &str, ';')) {
			fieldType = NULL;
		}
		else if (!dsNextChar(str, &str, ',')) {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
	}
	if (!dsCheckDuplicate(type->field->name,
		type->nField, sizeof(DS_FIELD_T))) {
		return FALSE;
	}
	if (!dsNextChar(str, &str, '}')) {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	type->size += DS_PAD(type->size, type->modulus);
	type->stdsize += DS_PAD(type->stdsize, type->stdmodulus);
	if (type->size != type->stdsize) {
		type->flags &= ~DS_STD_REP;
	}

	if (tag[0] != '\0') {
		if (dsFindTag(tag, mem)) {
			DS_ERROR(DS_E_DUPLICATE_STRUCT_TAG);
		}
		strcpy(type->name, tag);
	}

	if (ptr != NULL) {
		*ptr = str;
	}
	*pType = type;
	return TRUE;
}
/******************************************************************************
*
* dsDumpTypes - dump basicType struct
*
*/
int dsDumpTypes(void)
{
	size_t i;

	if (nBasic == 0) {
		dsBasicInit();
	}
	printf("name\tcode\tflags\tmod\tsize\tstdmod\tstdsize\tnField\tptr\n");
	for (i = 0; i < nBasic; i++) {
		printf("%s\t%d\t%X\t%d\t%d\t%d\t%d\t%d\t%X\n",
			basicType[i].name, basicType[i].code,
			basicType[i].flags, basicType[i].modulus,
			basicType[i].size, basicType[i].stdmodulus,
			basicType[i].stdsize, basicType[i].nField,
			(size_t)basicType[i].field);
	}
	return TRUE;
}
/******************************************************************************
*
* dsFindTag - find a structure tag and return a pointer to its type
*
*/
static DS_TYPE_T *dsFindTag(char *tag, DS_MEM_T *mem)
{
	DS_TYPE_T *type;

	type = (DS_TYPE_T *)mem->first;
	while ((char *)type < mem->next) {
		if (strcmp(tag, type->name) == 0) {
			return type;
		}
		type = (DS_TYPE_T *)&(type->field[type->nField]);
	}
	return NULL;
}
/******************************************************************************
*
*/
int dsFindField(DS_FIELD_T **ppField, DS_TYPE_T *pType, char *name)
{
	size_t i;

	if (pType->code != DS_TYPE_STRUCT) {
		DS_ERROR(DS_E_STRUCT_REQUIRED);
	}
	for (i = 0; i < pType->nField; i++) {
		if (strcmp(pType->field[i].name, name) == 0) {
			if (ppField != NULL) {
				*ppField = &pType->field[i];
			}
			return TRUE;
		}
	}
	DS_ERROR(DS_E_FIELD_NOT_FOUND);
}
/******************************************************************************
*
* dsFmtTypeDef - format a declaration string for a type structure
*
*/
int dsFmtTypeDef(char *str, size_t maxSize, DS_TYPE_T *type)
{
	size_t nTag = 0;
	DS_TYPE_T *tags[DS_MAX_STRUCT];
	DS_MEM_T bp;

	bp.first = bp.next = str;
	bp.limit = str + maxSize;
	if (	!dsStrToMem(&bp, "type ") ||
		!dsFmtTypeDefR(&bp, type, 0, &nTag, tags) ||
		!dsStrToMem(&bp, "\n")) {
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* dsFmtTypeDefR - recursive part of format type declaration string
*
*/
static int dsFmtTypeDefR(DS_MEM_T *bp,
	DS_TYPE_T *type, int level, size_t *pNTag, DS_TYPE_T **tag)
{
	size_t i, j;
	DS_TYPE_T *ft, *lastType = NULL;
	DS_FIELD_T *field;

	if (!dsIndent(bp, level)) {
		return FALSE;
	}
	if (type->code != DS_TYPE_STRUCT) {
		if (!dsNameToMem(bp, type->name)) {
			return FALSE;
		}
		return TRUE;
	}
	if (!dsStrToMem(bp, "struct ")) {
		return FALSE;
	}
	if (type->name[0] != '\0') {
		if (!dsNameToMem(bp, type->name)) {
			return FALSE;
		};
		for (i = 0; i < *pNTag; i++) {
			if (tag[i] == type)
				return TRUE;
		}
	}
	if (!dsStrToMem(bp, " {\n")) {
		return FALSE;
	}
	field = type->field;
	if (!dsCheckDuplicate(field->name, type->nField, sizeof(DS_FIELD_T))) {
		return FALSE;
	}
	for (i = 0; i < type->nField; i++) {
		ft = field[i].type;
		if (ft != lastType) {
			if (lastType && !dsStrToMem(bp, ";\n")) {
				return FALSE;
			}
			if (!dsFmtTypeDefR(bp, ft, level + 1, pNTag, tag)) {
				return FALSE;
			}
			lastType = ft;
		}
		else {
			if (!dsStrToMem(bp, ", ")) {
				return FALSE;
			}
		}
		if (!dsStrToMem(bp, " ") ||
			!dsNameToMem(bp, type->field[i].name)) {
			return FALSE;
		}

		for (j = 0; j < DS_MAX_DIMS && type->field[i].dim[j]; j++) {
			if (!dsStrToMem(bp, "[") ||
				!dsNumToMem(bp, type->field[i].dim[j]) ||
				!dsStrToMem(bp, "]")) {
				return FALSE;
			}
		}
	}
	if (!dsStrToMem(bp, ";\n") ||
		!dsIndent(bp, level) ||
		!dsStrToMem(bp, "}")) {
		return FALSE;
	}
	if (type->name[0] != '\0') {
		if ((i = *pNTag) >= DS_MAX_STRUCT) {
			DS_ERROR(DS_E_TOO_MANY_STRUCT_TAGS);
		}
		tag[i] = type;
		*pNTag = i + 1;
	}
	return TRUE;
}
/******************************************************************************
*
* dsTypeCmp - compare two type structures
*
*/
int dsTypeCmp(DS_TYPE_T *t1, DS_TYPE_T *t2)
{
	int c, i;
	DS_FIELD_T *f1, *f2, *limit;

	if ((c = strcmp(t1->name, t2->name)) != 0) return c;
	if (t1->code - t2->code) return (t1->code - t2->code);
	if (t1->size - t2->size) return (t1->size - t2->size);
	if (t1->modulus - t2->modulus) return (t1->modulus - t2->modulus);
	if (t1->stdsize - t2->stdsize) return (t1->stdsize - t2->stdsize);
	if (t1->stdmodulus - t2->stdmodulus) {
		return (t1->stdmodulus - t2->stdmodulus);
	}
	if (t1->nField - t2->nField) return (t1->nField - t2->nField);

	f1 = t1->field;
	limit = f1 + t1->nField;
	f2 = t2->field;
	for (;f1 < limit; f1++, f2++) {
		if ((c = strcmp(f1->name, f2->name)) != 0) return c;
		if (f1->count - f2->count) return (f1->count - f2->count);
		if (f1->offset - f2->offset) return (f1->offset - f2->offset);

		for (i = 0; i < DS_MAX_DIMS; i++) {
			if (f1->dim[i] != f2->dim[i]) {
				return (f1->dim[i] - f2->dim[i]);
			}

			if (f1->dim[i] == 0) break;
		}
		if ((c = dsTypeCmp(f1->type, f2->type)) != 0) {
			return c;
		}
	}
	return 0;
}
