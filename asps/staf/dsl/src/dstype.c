/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dstype.c - routines to parse a subset of CORBA style types */

/*
modification history
--------------------
12feb95,whg	written.
11jun96,whg  added indirection to dataset structure
*/

/*
DESCRIPTION
These routines parse struct defs defined by the CORBA IDL version 2
*/
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#define DS_PRIVATE
#define DS_GLOBAL_ONE
#include "dstype.h"
/******************************************************************************
*
* descriptor for memory region
*
*/
typedef struct {
	char *first;	/* address of memory region */
	char *next;		/* next free byte of region */
	char *limit;	/* address of last byte of region plus one */
	int *map;		/* number of fields in type */
}DS_MEM_T;
/*****************************************************************************
*
* prototypes for static functions
*
*/
static int dsBasicInit(void);
static int dsFirstPass(int *pSepCount, int *map,
	int *pMapCount, int maxMapCount, char *str);  /* str modif inside*/
static int dsFormatTypeSpecifierR(DS_BUF_T *bp,
	DS_TYPE_T *type, int level, size_t *pNTag, DS_TYPE_T **tag);
static int dsParseTypeR(DS_TYPE_T **pType, DS_BUF_T *bp,
	DS_MEM_T *mem, DS_TYPE_T **scope, unsigned level);
static int dsSearchScope(DS_TYPE_T **ppType, char *name,
	DS_TYPE_T **scope, unsigned level, int top);
/******************************************************************************
*
* basicType - definition of basic types
*
*/
DS_MODULUS_STRUCT(1, DS_TYPE_CHAR,    char);
DS_MODULUS_STRUCT(1, DS_TYPE_OCTET,   octet);
DS_MODULUS_STRUCT(2, DS_TYPE_SHORT,   short);
DS_MODULUS_STRUCT(2, DS_TYPE_U_SHORT, unsigned short);

/* 12 December 1997 - fixed basicType definition for 64-bit Alpha
                      long and ulong are 8 bytes (not 4!)
		      John Lajoie - lajoie@iastate.edu
*/
#if defined(__alpha)
DS_MODULUS_STRUCT(8, DS_TYPE_LONG,    long);
DS_MODULUS_STRUCT(8, DS_TYPE_U_LONG,  unsigned long);
#else
DS_MODULUS_STRUCT(4, DS_TYPE_LONG,    long);
DS_MODULUS_STRUCT(4, DS_TYPE_U_LONG,  unsigned long);
#endif

DS_MODULUS_STRUCT(4, DS_TYPE_FLOAT,   float);
DS_MODULUS_STRUCT(8, DS_TYPE_DOUBLE,  double);

static DS_TYPE_T basicType[] = {
	DS_TYPE_INIT(1, DS_TYPE_CHAR,    char),
	DS_TYPE_INIT(1, DS_TYPE_OCTET,   octet),
	DS_TYPE_INIT(2, DS_TYPE_SHORT,   short),
	DS_TYPE_INIT(2, DS_TYPE_U_SHORT, unsigned short),

/* 12 December 1997 - fixed basicType definition for 64-bit Alpha
                      long and ulong are 8 bytes (not 4!)
		      John Lajoie - lajoie@iastate.edu
*/
#if defined(__alpha)
	DS_TYPE_INIT(8, DS_TYPE_LONG,    long),
	DS_TYPE_INIT(8, DS_TYPE_U_LONG,  unsigned long),
#else
	DS_TYPE_INIT(4, DS_TYPE_LONG,    long),
	DS_TYPE_INIT(4, DS_TYPE_U_LONG,  unsigned long),
#endif

	DS_TYPE_INIT(4, DS_TYPE_FLOAT,   float),
	DS_TYPE_INIT(8, DS_TYPE_DOUBLE,  double)};
	
static size_t nBasic = 0;
/******************************************************************************
*
* dsBasicInit - initialize basic types
*
* RETURNS: TRUE if success else FALSE
*/
static int dsBasicInit()
{
	size_t i, n = sizeof(basicType)/sizeof(basicType[0]);
	DS_TYPE_T *t;

	/********** start critical section *******************************/
	if (!dsTypeSemTake()) {
		return FALSE;
	}
	if (nBasic != 0) {
		return dsTypeSemGive();
	}
	for (i = 0, t = basicType; i < n; i++, t++) {
		if (i != (size_t)t->code || t->flags != 0 || t->modulus == 0 ||
			t->size != t->stdsize || t->size != t->stdmodulus || 
			t->nField != 0) {
			goto fail;		
		}
		if ((t->size > 1 && !DS_IS_BIG_ENDIAN) ||
			(DS_IS_REAL(t) && !DS_IS_IEEE_FLOAT)) {
			t->flags = DS_NOT_STD_REP;
		}
	}
	nBasic = n;
	return dsTypeSemGive();
	
fail:
	if (!dsTypeSemGive()) {
		return FALSE;
	}
	/********** end critical section *******************************/
	DS_ERROR(DS_E_SYSTEM_ERROR);
}
/******************************************************************************
*
* dsCheckDupField - verify field names are unique 
*
* RETURNS: TRUE if unique names, FALSE if duplicate or names collide
*/
static int dsCheckDupField(DS_TYPE_T *pType)
{
	int c, n;
	DS_FIELD_T *f1, *f2, *last;

	if ((n = pType->nField - 1) > 0) {
		for (f1 = DS_FIELD_PTR(pType), last = f1 + n; f1 < last; f1++) {
			for (f2 = f1 + 1; f2 <= last; f2++) {
				if ((c = dsCmpName(f1->name, f2->name)) <= 0) {
					if (c < 0) {
						DS_ERROR(DS_E_NAMES_COLLIDE);
					}
					else {
						DS_ERROR(DS_E_DUPLICATE_NAME);
					}
				}
			}
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsCmpFieldType - compare two fields for project compatibility
*
* RETURN: zero if equal, one if not equal
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
* dsDumpTypes - dump basicType struct
*
* RETURNS: TRUE
*/
int dsDumpTypes(void)
{
	char c = 0;
	size_t i;

	if (nBasic == 0) {
		dsBasicInit();
	}
	printf("char type is %s\n", --c > 0 ? "unsigned": "signed");
	printf("addressing is %s\n",
		DS_IS_BIG_ENDIAN ? "BIG_ENDIAN" : "LITTLE_ENDIAN");
	printf("floating point is %s \n", DS_IS_IEEE_FLOAT ? "IEEE" : "UNKNOWN");
	printf("code\tflags\tmod\tsize\tstdmod\tstdsize\tnField\tname\n");
	for (i = 0; i < nBasic; i++) {
		printf("%d\t%X\t%d\t%d\t%d\t%d\t%d\t%s\n",
			basicType[i].code, basicType[i].flags, basicType[i].modulus,
			basicType[i].size, basicType[i].stdmodulus,
			basicType[i].stdsize, basicType[i].nField, basicType[i].name);
	}
	return TRUE;
}
/******************************************************************************
*
* dsFindField - return a pointer to a named field
*
* RETURN: -1 if name collision, 0 if found or 1 if not found
*/
int dsFindField(DS_FIELD_T **ppField, DS_TYPE_T *pType, const char *name)
{
	int c;
	size_t i;
	DS_FIELD_T *field;

	field = DS_FIELD_PTR(pType);
	for (i = 0; i < pType->nField; i++) {
		if (( c = dsCmpName(field[i].name, name)) <= 0) {
			*ppField = &field[i];
			return c;
		}
	}
	return 1;
}
/******************************************************************************
*
* dsFirstPass - first pass parse of type specifier to get memory map
*
* RETURNS: TRUE for success or FALSE for error
*/
static int dsFirstPass(int *pSepCount, int *map,
	int *pMapCount, int maxMapCount, char *str)
{
	int mapCount = 0, nest = -1, sepCount = 0, stack[DS_MAX_NEST];

	for (; isalnum(*str) || isspace(*str) || *str == '_'; str++);

	if (*str != '{') {
		*pMapCount = mapCount;
		*pSepCount = 0;
		return TRUE;
	}
	for (;;) {
		for (; isalnum(*str) || isspace(*str) || *str == '_'; str++);
		switch (*str++) {

			case ';':
			case ',':
				map[stack[nest]]++;
				sepCount++;
				break;

			case '{':
				if (++nest >= DS_MAX_NEST) {
					DS_ERROR(DS_E_NESTED_TOO_DEEP);
				}
				stack[nest] = mapCount++;
				if (mapCount > maxMapCount) {
					DS_ERROR(DS_E_TOO_MANY_MEMBERS);
				}
				map[stack[nest]] = 0;
				break;

			case '}':
				if (nest == 0) {
					*pMapCount = mapCount;
					*pSepCount = sepCount;
					return TRUE;
				}
				nest--;
				break;

			case '[':
				while(*str != '\0' && *str++ != ']');
				break;

			default: 
				DS_ERROR(DS_E_SYNTAX_ERROR);
		}
	}
}
/******************************************************************************
*
* dsFormatTypeSpecifier - format a declaration string for a structure
*
* RETURNS: TRUE if success else FALSE
*/
int dsFormatTypeSpecifier(char *str, size_t maxSize, DS_TYPE_T *type)
{
	size_t nTag = 0;
	DS_TYPE_T *tags[DS_MAX_STRUCT];
	DS_BUF_T bp;

	DS_PUT_INIT(&bp, str, maxSize);
	if (!dsFormatTypeSpecifierR(&bp, type, 0, &nTag, tags)) {
		return FALSE;
	}
	if (dsPutc('\0', &bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	return TRUE;
}
/******************************************************************************
*
* dsFormatTypeSpecifierR - recursive part of format type declaration string
*
* RETURNS: TRUE if success else FALSE
*/
static int dsFormatTypeSpecifierR(DS_BUF_T *bp,
	DS_TYPE_T *type, int level, size_t *pNTag, DS_TYPE_T **tag)
{
	size_t i, j;
	DS_TYPE_T *ft, *lastType = NULL;
	DS_FIELD_T *field;

	if (dsPutTabs(level, bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (type->code != DS_TYPE_STRUCT) {
		if (dsPuts(type->name, bp) < 0) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}
		return TRUE;
	}
	for (i = 0; i < *pNTag; i++) {
		if (tag[i] == type) {
			if (dsPuts(type->name, bp) < 0) {
				DS_ERROR(DS_E_ARRAY_TOO_SMALL);
			}
			return TRUE;
		}
	}
	if (dsPuts("struct ", bp) < 0 ||
		dsPuts(type->name, bp) < 0 ||
		dsPuts(" {\n", bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
	}
	if (!dsCheckDupField(type)) {
		return FALSE;
	}
	for (field = DS_FIELD_PTR(type), i = 0; i < type->nField; i++) {
		ft = field[i].type;
		if (ft != lastType) {
			if (lastType && dsPuts(";\n", bp) < 0) {
				DS_ERROR(DS_E_ARRAY_TOO_SMALL);
			}
			if (!dsFormatTypeSpecifierR(bp, ft, level + 1, pNTag, tag)) {
				return FALSE;
			}
			lastType = ft;
		}
		else {
			if (dsPuts(", ", bp) < 0) {
				DS_ERROR(DS_E_ARRAY_TOO_SMALL);
			}
		}
		if (dsPutc(' ', bp) < 0 || dsPuts(field[i].name, bp) < 0) {
			DS_ERROR(DS_E_ARRAY_TOO_SMALL);
		}

		for (j = 0; j < DS_MAX_DIMS && field[i].dim[j]; j++) {
			if (dsPutc('[', bp) < 0 ||
				dsPutNumber(field[i].dim[j], bp) < 0 ||
				dsPutc(']', bp) < 0) {
				DS_ERROR(DS_E_ARRAY_TOO_SMALL);
			}
		}
	}
	if (dsPuts(";\n", bp) < 0|| dsPutTabs(level, bp) < 0 ||
		dsPuts("}", bp) < 0) {
		DS_ERROR(DS_E_ARRAY_TOO_SMALL);
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
* dsParseType - create a type structure for a type declaration string
*
* RETURNS: TRUE if success else FALSE
*/
int dsParseType(DS_TYPE_T **pType, size_t *pSize, const char *str, const char **ptr)
{
	int map[DS_MAX_STRUCT], nMap, nSep;
	size_t size;
	DS_BUF_T bp;
	DS_MEM_T mem;
	DS_TYPE_T *scope[DS_MAX_NEST], *type;

	DS_GET_INIT(&bp, str)
	if (nBasic == 0  && !dsBasicInit()) {
		return FALSE;
	}
	if (!dsFirstPass(&nSep, map, &nMap, DS_MAX_STRUCT, (char*)str)) {
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
	if (!dsParseTypeR(&type, &bp, &mem, scope, 0)) {
		goto fail;
	}
	if (type->code != DS_TYPE_STRUCT) {
		DS_LOG_ERROR(DS_E_STRUCT_REQUIRED);
		goto fail;
	}
	if (mem.limit != mem.next) {
		DS_LOG_ERROR(DS_E_SYSTEM_ERROR);
		goto fail;
	}
	if (pSize != NULL) {
		*pSize = size;
	}
	if (ptr) {
		*ptr = bp.out;
	}
	*pType = type;
	return TRUE;
fail:
	if (*pType == NULL) {
		dsTypeFree(mem.first, size);
	}
	return FALSE;
}
/******************************************************************************
*
* dsParseTypeR - second pass for parse of type declaration
*
* RETURNS: TRUE if success else FALSE
*/
static int dsParseTypeR(DS_TYPE_T **pType, DS_BUF_T *bp,
	DS_MEM_T *mem, DS_TYPE_T **scope, unsigned level)
{
	char name[DS_NAME_DIM], tmp[DS_NAME_DIM];
	int c, j, nField;
	size_t dim, i, isUnsigned;
	DS_FIELD_T *field;
	DS_TYPE_T *fieldType, *type;

	if (dsGetName(name, bp) < 0) {
		DS_ERROR(DS_E_INVALID_TYPE_NAME);
	}
	if ((c = dsCmpName("unsigned", name)) == 0) {
 	 	if (dsGetName(tmp, bp) < 0 ||
  			(strlen(tmp) + strlen(name)) >= DS_MAX_NAME_LEN) {
  	 		DS_ERROR(DS_E_INVALID_TYPE_NAME);
  	 	}
  	 	strcat(name, " ");
  	 	strcat(name, tmp);
  	 	isUnsigned = TRUE;
  	}
  	else {
		if (c < 0) {
			DS_ERROR(DS_E_NAMES_COLLIDE);
  	 	}
  		isUnsigned = FALSE;
  	}
  	for (i = 0; i < nBasic; i++) {
  		if ((c = dsCmpName(name, basicType[i].name)) <= 0) {
			if (c < 0) {
				DS_ERROR(DS_E_NAMES_COLLIDE);
	  	 	}
	  	 	*pType = &basicType[i];
	  	 	return TRUE;
	  	 }
	}
	if (isUnsigned) {
		DS_ERROR(DS_E_INVALID_TYPE_NAME);
	}
	if ((c = dsCmpName(name, "struct")) != 0) {
		if (c < 0 ) {
			DS_ERROR(DS_E_NAMES_COLLIDE);
		}
		if (dsSearchScope(&type, name, scope, level, FALSE) != 0) {	
			DS_ERROR(DS_E_INVALID_TYPE_NAME);
		}
		*pType = type;
		return TRUE;
	}
	if (dsGetName(name, bp) < 0) {
		DS_ERROR(DS_E_INVALID_TYPE_NAME);
	}
	if (dsGetNonSpace(bp) != '{') {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	if (dsSearchScope(&type, name, scope, level, TRUE) <= 0) {
		DS_ERROR(DS_E_STRUCT_REDEFINITION);
	}
	type = (DS_TYPE_T *)mem->next;
	field = DS_FIELD_PTR(type);
	nField = *mem->map++;
	if (nField == 0) {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	mem->next = (char *)&field[nField];
	if (mem->next > mem->limit) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	type->code = DS_TYPE_STRUCT;
	type->nField = nField;
	if (level >= DS_MAX_NEST) {
		DS_ERROR(DS_E_NESTED_TOO_DEEP);
	}
	scope[level] = type;
	for(i = type->nField, fieldType = NULL; i-- > 0 ; field++) {
		if (fieldType == NULL) {
			if (!dsParseTypeR(&fieldType, bp, mem, scope, level + 1)) {
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
				type->flags |= DS_NOT_STD_REP;
			}
		}
		if (dsGetName(field->name, bp) < 0) {
			DS_ERROR(DS_E_INVALID_NAME);
		}
		field->type = fieldType;
		field->count = 1;
		for (j = 0; (c = dsGetNonSpace(bp)) == '['; j++) {
			if (j == DS_MAX_DIMS) {
				DS_ERROR(DS_E_TOO_MANY_DIMENSIONS);
			}
			if (dsGetNumber(&dim, bp) < 0 ||
				 dim < 1 || dsGetNonSpace(bp) != ']') {
				DS_ERROR(DS_E_INVALID_DIMENSION);
			}
			field->dim[j] = (unsigned)dim;
			field->count *= (unsigned)dim;
		}
		field->offset = type->size;
		field->stdoffset = type->stdsize;
		type->size += field->count*fieldType->size;
		type->stdsize += field->count*fieldType->stdsize;

		if (c == ';') {
			fieldType = NULL;
		}
		else if (c != ',') {
			DS_ERROR(DS_E_SYNTAX_ERROR);
		}
	}
	if (!dsCheckDupField(type)) {
		return FALSE;
	}
	if (dsGetNonSpace(bp) != '}') {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	type->size += DS_PAD(type->size, type->modulus);
	type->stdsize += DS_PAD(type->stdsize, type->stdmodulus);
	if (type->size != type->stdsize) {
		type->flags |= DS_NOT_STD_REP;
	}
	strcpy(type->name, name);
	*pType = type;
	return TRUE;
}
/******************************************************************************
*
* dsSearchScope - look for type name in fields of scope
*
* RETURNS: one if not found, zero if found, -1 if name collision
*/
static int dsSearchScope(DS_TYPE_T **ppType, char *name,
	DS_TYPE_T **scope, unsigned level, int top)
{
	unsigned c, i;
	DS_FIELD_T *field;
	DS_TYPE_T *type;

	for (; level-- > 0;) {
		type = scope[level];
		field = DS_FIELD_PTR(type);
		for (i = 0; i < type->nField && field[i].type; i++) {
			if ((c = dsCmpName(name, field[i].type->name)) <= 0) {
				if (c == 0) {
					*ppType =  field[i].type;
				}
				return c;
			}
		}
		if (top) {
			break;
		}
	}
	return 1;
}	
/******************************************************************************
*
* dsTypeCmp - compare two type structures
*
* RETURN: zero if equal, negative if t1 < t2. posative if t1 > t2
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

	f1 = DS_FIELD_PTR(t1);
	limit = f1 + t1->nField;
	f2 = DS_FIELD_PTR(t2);
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
