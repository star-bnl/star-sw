/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dstype.c - routines to parse a subset of CORBA style types */

/*
modification history
--------------------
12feb95,whg	written.
11jun96,whg added indirection to dataset structure
12jun98,whg updated to allow C++ wrappers
*/

/*
DESCRIPTION
These routines parse struct defs defined by the CORBA IDL version 2
*/
/* the grammer:

struct_type:
	STRUCT IDENT '{' field_list '}'
	;
field_list:
	declaration
	| field_list declaration
	;
declaration:
	type_specifier declarator_list ';'
	;
declarator_list:
	declarator
	| declarator_list ',' declarator
	;
declarator:
	IDENT
	| declarator '[' NUM ']'
	;
type_specifier:
	base_type
	| IDENT
	| struct_type
	;
base_type:
	CHAR
	| OCTET
	| SHORT
	| UNSIGNED SHORT
	| LONG
	| UNSIGNED LONG
	| FLOAT
	| DOUBLE
	| LONG DOUBLE
	| LONG LONG
	| UNSIGNED LONG LONG
	;
*/
#include <ctype.h>
#include <stddef.h>
#include <string.h>
#define DS_PRIVATE
#define DS_GLOBAL_ONE
#include "dstype.h"
/******************************************************************************
*
* static structures for types
*
*/
/* replace by real semaphore for multi thread */
#define DS_TYPE_GIVE (++semGive == semTake)
#define DS_TYPE_TAKE (semTake++ == semGive)
static int semGive = 0, semTake = 0;
/*
 * macros to initialize basic types
 */
struct STRUCT_MOD_T {char a; struct t2 {char b;}c;};
#define DS_STRUCT_MODULUS offsetof(struct STRUCT_MOD_T, c)

#define DS_TYPE_DEF(v, t, n) struct t ## _MOD_T {char x;  DS_ ## t y;};\
	static DS_TYPE_T v = {#n, DS_TYPE_ ## t, sizeof(DS_ ## t),\
	offsetof(struct t ## _MOD_T , y), DS_LEN_ ## t, DS_LEN_ ## t,\
	 0, 0, 0, 0, 0, 0};
 /*
  * Basic types
  */
DS_TYPE_DEF(dsCharType,   CHAR,    char)
DS_TYPE_DEF(dsOctetType,  OCTET,   octet)
DS_TYPE_DEF(dsShortType,  SHORT,   short)
DS_TYPE_DEF(dsUShortType, U_SHORT, unsigned short)
DS_TYPE_DEF(dsLongType,   LONG,    long)
DS_TYPE_DEF(dsULongType,  U_LONG,  unsigned long)
DS_TYPE_DEF(dsFloatType,  FLOAT,   float)
DS_TYPE_DEF(dsDoubleType, DOUBLE,  double)

static DS_TYPE_T *basicType[] = {&dsCharType, &dsOctetType, &dsShortType,
&dsUShortType, &dsLongType, &dsULongType, &dsFloatType, &dsDoubleType};

static size_t nBasic = 0;
/*
 *  hash struct for types
 */
struct type_node_t {
	size_t next;
	DS_TYPE_T *type;
};
static size_t *typeHash = NULL;
static struct type_node_t *typeNode = NULL;
static unsigned nodeCount = 0;
/******************************************************************************
*
* dsBasicInit - initialize basic types
*
* RETURNS: TRUE if success else FALSE
*/
static int dsBasicInit()
{
	float floatOne = 1.0f;
	int intOne = 1;
	size_t i;
	DS_TYPE_T *t;

	/********** start critical section *******************************/
	if (!DS_TYPE_TAKE) {
		DS_ERROR(DS_E_SEM_TAKE_ERROR);
	}
	if (nBasic != 0) {
		DS_TYPE_GIVE;
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	nBasic = sizeof(basicType)/sizeof(basicType[0]);
	if (DS_LEN_CHAR != sizeof(DS_CHAR) || DS_LEN_OCTET != sizeof(DS_OCTET) ||
		DS_LEN_FLOAT != sizeof(DS_FLOAT) || DS_LEN_DOUBLE != sizeof(DS_DOUBLE)) {
		goto fail;
	}
	dsIsBigEndian = (((char *)&intOne)[sizeof(intOne)-1] == 1);
	i = sizeof(float);
	dsIsIeee = ((sizeof(int) == i && ((int *)&floatOne)[0] == DS_IEEEF_ONE) ||
		(sizeof(short) == i && ((short *)&floatOne)[0] == DS_IEEEF_ONE));
	for (i = 0; i < nBasic; i++) {
		t = basicType[i];
		if (t->flags != 0 ||  t->nField != 0 ||t->modulus == 0 ||
			t->size == 0 || t->size < t->stdsize) {
			goto fail;		
		}
		if (t->size > 1) t->flags |= DS_MULTI_BYTE;
		if (t->size != t->stdsize || (DS_IS_REAL(t) && !DS_IS_IEEE_FLOAT)) {
			t->flags |= DS_NOT_STD_REP;
		}
	}
	typeHash = dsTypeCalloc(sizeof(typeHash[0])*DS_TYPE_HASH_DIM);
	typeNode = dsTypeCalloc(sizeof(typeNode[0])*DS_TYPE_NODE_DIM);
	if (typeHash == NULL || typeNode == NULL) {
		goto fail;
	}
	nodeCount = 1;
	/********** end critical section *******************************/
	if (DS_TYPE_GIVE) {
		return TRUE;
	}
	DS_ERROR(DS_E_SEM_GIVE_ERROR);
	
fail:
	DS_TYPE_GIVE;

	DS_ERROR(DS_E_TYPE_REPRESENTATION_ERROR);
}
/******************************************************************************
*
* dsCountFields - count fields in a struct specifier
*
* RETURNS: number of fields or -1 for error
*/
static int dsCountFields(char *str)
{
	int nest, nField;

	for (; isalnum(*str) || isspace(*str) || *str == '_'; str++);

	if (*str != '{') {
		return -1;
	}
	for (nest = nField = 0;;) {
		for (; isalnum(*str) || isspace(*str) || *str == '_'; str++);
		switch (*str++) {

			case ';':
			case ',':
				if (nest == 1) {
					nField++;
				}
				break;

			case '{':
				nest++;
				break;

			case '}':
				if (nest-- == 1) {
					return nField;
				}
				break;

			case '[':
				while(*str != '\0' && *str++ != ']');
				break;

			default:
				return -1;
		}
	}
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
	DS_TYPE_T *t;

	if (nBasic == 0 && !dsBasicInit()) {
		dsPerror("initialization failed");
		return FALSE;
	}
	printf("\nchar type is %s\n", --c > 0 ? "unsigned": "signed");
	printf("addressing is %s\n",
		DS_IS_BIG_ENDIAN ? "BIG_ENDIAN" : "LITTLE_ENDIAN");
	printf("floating point is %s \n", DS_IS_IEEE_FLOAT ? "IEEE" : "UNKNOWN");
	printf("minimum struct modulus is %d\n", DS_STRUCT_MODULUS);
	printf("\ncode\tflags\tmod\tsize\tstdmod\tstdsize\tname\n");
	for (i = 0; i < nBasic; i++) {
		t = basicType[i];
		printf("%d\t%X\t%d\t%d\t%d\t%d\t%s\n", t->code, t->flags,
			t->modulus, t->size, t->stdmodulus, t->stdsize, t->name);
	}
	printf("\n");
	return TRUE;
}
/*****************************************************************************
*
* dsHashType - maintain master type lists
*
* RETURNS: pointer to master struct for type or NULL if error
*/
static DS_TYPE_T *dsHashType(DS_TYPE_T *pType)
{
	size_t h, *link, tid;
	
	if (pType->tid != 0) {
		DS_LOG_ERROR(DS_E_SYSTEM_ERROR);
		return NULL;
	}
	h = dsHash(pType->name)%DS_TYPE_HASH_DIM;
	/********** start critical section *******************************/
	if (!DS_TYPE_TAKE) {
		DS_LOG_ERROR(DS_E_SEM_TAKE_ERROR);
		return NULL;
	}
	for (link = &typeHash[h]; (tid = *link) != 0; link = &typeNode[tid].next) {
		if (dsTypeCmp(typeNode[tid].type, pType) == 0) {
			goto done;
		}
	}
	if (nodeCount < DS_TYPE_NODE_DIM) {
		*link = nodeCount;
		tid = nodeCount++;
		typeNode[tid].type = pType;
		pType->tid = tid;
	}
	else {
		DS_LOG_ERROR(DS_E_TOO_MANY_TYPES);
		tid = 0;
	}
done:
	/********** end critical section *******************************/
	if (!DS_TYPE_GIVE) {
		DS_LOG_ERROR(DS_E_SEM_GIVE_ERROR);
		tid = 0;
	}
	return tid ? typeNode[tid].type : NULL;
}
/*****************************************************************************
*
* dsIsReserved - check for reserved name
*
* RETURNS: TRUE for reserved name else FALSE
*/
static int dsIsReserved(char *name)
{
	switch(tolower(*name)) {
	case 'c': return dsCmpName(name, "char") < 1;
	case 'd': return dsCmpName(name, "double") < 1;
	case 'f': return dsCmpName(name, "float") < 1;
	case 'l': return dsCmpName(name, "long") < 1;
	case 'o': return dsCmpName(name, "octet") < 1;
	case 's': return dsCmpName(name, "short") < 1 ||
					 dsCmpName(name, "struct") < 1;
	case 'u': return dsCmpName(name, "unsigned") < 1;
	default: return FALSE;
	}
}
/******************************************************************************
*
* dsParseSimple - parse basic type or scoped name
*
* RETURNS: TRUE if success else FALSE
*/
static int dsParseSimple(DS_TYPE_T **pType, char *str, char **ptr,
						 DS_TYPE_T **scope, size_t nScope)
{
	int i;

	for(;isspace(*str); str++);
	if (dsIsNextName("char", str, &str)) {
		*pType = &dsCharType;
	}
	else if (dsIsNextName("double", str, &str)) {
		*pType = &dsDoubleType;
	}
	else if (dsIsNextName("float", str, &str)) {
		*pType = &dsFloatType;
	}
	else if (dsIsNextName("long", str, &str)) {
		if (dsIsNextName("double", str, &str)) {
			DS_ERROR(DS_E_LONG_DOUBLE_NOT_SUPPORTED);
		}
		else if (dsIsNextName("long", str, &str)) {
			DS_ERROR(DS_E_LONG_LONG_NOT_SUPPORTED);
		}
		else {
			*pType = &dsLongType;
		}
	}
	else if (dsIsNextName("octet", str, &str)) {
		*pType = &dsOctetType;
	}
	else if (dsIsNextName("short", str, &str)) {
		*pType = &dsShortType;
	}
	else if (dsIsNextName("unsigned", str, &str)) {
		if (dsIsNextName("short", str, &str)) {
			*pType = &dsUShortType;
		}
		else if (dsIsNextName("long", str, &str)) {
			if (dsIsNextName("long", str, &str)) {
				DS_ERROR(DS_E_UNSIGNED_LONG_LONG_NOT_SUPPORTED);
			}
			else {
				*pType = &dsULongType;
			}
		}
		else {
			DS_ERROR(DS_E_INVALID_TYPE_NAME);
		}
	}
	else {
		for (i = nScope; i-- > 0;) {
			if (dsIsNextName(scope[i]->name, str, &str)) {
				break;
			}
		}
		if (i < 0) {
			DS_ERROR(DS_E_INVALID_TYPE_NAME);
		}
		*pType = scope[i];
	}
	*ptr = str;
	return TRUE;
}
/******************************************************************************
*
* dsParseStruct - parse struct
*
* RETURNS: TRUE if success else FALSE
*/
static int dsParseStruct(DS_TYPE_T **pType, char *str, char **ptr,
	DS_TYPE_T **scope, size_t nScope)
{
	char name[DS_NAME_DIM];
	int c;
	size_t dim, i, j, nField, size;
	DS_FIELD_T *field;
	DS_TYPE_T *fieldType, *hashType, *type;

	if (!dsParseName(name, str, &str) || strcmp(name, "struct") != 0 ||
		!dsParseName(name, str, &str) || dsIsReserved(name)) {
		DS_ERROR(DS_E_INVALID_TYPE_NAME);
	}
	nField = dsCountFields(str);
	if (nField < 1 || dsNonSpace(str, &str) != '{') {
		DS_ERROR(DS_E_SYNTAX_ERROR);
	}
	size = sizeof(DS_TYPE_T) + nField*sizeof(DS_FIELD_T);
	if ((type = dsTypeCalloc(size)) == NULL) {
		return FALSE;
	}
	strcpy(type->name, name);
	type->field = (DS_FIELD_T *)&type[1];
	type->code = DS_TYPE_STRUCT;
	type->modulus = DS_STRUCT_MODULUS;
	type->nField = nField;
	for(fieldType = NULL, i = 0; i < nField; i++) {
		field = &type->field[i];
		if (fieldType == NULL) {
			if (!dsIsNextName("struct", str, NULL)) {
				if (!dsParseSimple(&fieldType, str, &str, scope, nScope)) {
					goto fail;
				}
			}
			else {
				if (!dsParseStruct(&fieldType, str, &str, scope, nScope)) {
					goto fail;
				}
				for (j = 0; j < i; j++) {
					if (dsCmpName(fieldType->name, type->field[j].type->name) < 1) {
						DS_LOG_ERROR(DS_E_STRUCT_REDEFINITION);
						goto fail;
					}
				}
				for (j = 0; j < nScope; j++) {
					if (dsCmpName(scope[j]->name, fieldType->name) < 0) {
						DS_LOG_ERROR(DS_E_NAMES_COLLIDE);
						goto fail;
					}
				}
				if (nScope >= DS_SCOPE_DIM) {
					DS_LOG_ERROR(DS_E_SCOPE_TOO_LARGE);
					goto fail;
				}
				scope[nScope++] = fieldType;
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
			if (!DS_IS_MULTI_BYTE(fieldType)) {
				type->flags |= DS_MULTI_BYTE;
			}
		}
		if (!dsParseName(field->name, str, &str) ||
			dsIsReserved(field->name)) {
			DS_LOG_ERROR(DS_E_INVALID_NAME);
			goto fail;
		}
		for (j = 0; j < i; j++) {
			if ((c = dsCmpName(field->name, type->field[j].name)) < 1) {
				if (c < 0) {
					DS_LOG_ERROR(DS_E_NAMES_COLLIDE);
				}
				else {
					DS_LOG_ERROR(DS_E_DUPLICATE_NAME);
				}
				goto fail;
			}
		}
		field->type = fieldType;
		field->count = 1;
		for (j = 0; (c = dsNonSpace(str, &str)) == '['; j++) {
			if (j == DS_MAX_DIMS) {
				DS_LOG_ERROR(DS_E_TOO_MANY_DIMENSIONS);
				goto fail;
			}
			if (!dsParseNumber(&dim, str, &str) ||
				 dim < 1 || dsNonSpace(str, &str) != ']') {
				DS_LOG_ERROR(DS_E_INVALID_DIMENSION);
				goto fail;
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
			DS_LOG_ERROR(DS_E_SYNTAX_ERROR);
			goto fail;
		}
	}
	if (dsNonSpace(str, &str) != '}') {
		DS_LOG_ERROR(DS_E_SYNTAX_ERROR);
		goto fail;
	}
	type->size += DS_PAD(type->size, type->modulus);
	type->stdsize += DS_PAD(type->stdsize, type->stdmodulus);
	if (type->size != type->stdsize) {
		type->flags |= DS_NOT_STD_REP;
	}
	if ((hashType = dsHashType(type)) == NULL) {
		goto fail;
	}
	*ptr = str;
	if (type != hashType) {
		dsTypeFree(type, size);
		*pType = hashType;
	}
	else {
		*pType = type;
	}
	return TRUE;
fail:
	dsTypeFree(type, size);
	return FALSE;
}
/******************************************************************************
*
* dsParseType - create a type structure for a type declaration string
*
* RETURNS: TRUE if success else FALSE
*/
int dsParseType(DS_TYPE_T **pType, char *str, char **ptr)
{
	DS_TYPE_T *scope[DS_SCOPE_DIM], *type;

	if (nBasic == 0  && !dsBasicInit()) {
		return FALSE;
	}
	if (!dsParseStruct(&type, str, &str, scope, 0)) {
	return FALSE;
	}
	if (ptr) {
		*ptr = str;
	}
	*pType = type;
	return TRUE;
}
/******************************************************************************
*
* dsTypeCmp - compare two type structures
*
* RETURN: zero if equal, negative if t1 < t2. positive if t1 > t2
*/
int dsTypeCmp(DS_TYPE_T *t1, DS_TYPE_T *t2)
{
	int c, i;
	DS_FIELD_T *f1, *f2, *limit;

	if(t1 == t2) return 0;
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
/******************************************************************************
*
* dsTypePtr - get pointer to a type structure for a tid
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypePtr(DS_TYPE_T **pType, size_t tid)
{
	if (tid < 1 || tid >= nodeCount) {
printf("tid %d, nodeCount %d\n", tid, nodeCount);
		DS_ERROR(DS_E_INVALID_TYPE_ID);
	}
	*pType = typeNode[tid].type;
	return TRUE;
}
