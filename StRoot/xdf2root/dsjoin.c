/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dsjoin.c - routines that do joins and projects */

/*
modification history
--------------------
14mar95,whg  collected from other files 
*/

/*
DESCRIPTION
relation database join and project operations for tables
*/
#define DS_PRIVATE
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "dstype.h"

static int dsEquijoinFields(DS_FIELD_T **srcField, size_t *srcIndex,
	DS_TYPE_T **types, DS_KEY_T *key, char **names, char *projectList);
static int dsEquijoinKey(DS_KEY_T *key,
	DS_TYPE_T **types, char **names, char *joinList);
static int dsProjectFields(DS_FIELD_T **srcField, size_t *srcIndex,
	DS_TYPE_T *dstType, DS_TYPE_T **srcType, char **names, char *projectList);
/*****************************************************************************
*
* dsEquijoin - construct projection of natural join
*
* RETURNS: TRUE if success else FALSE
*/
int dsEquijoin(DS_DATASET_T *pJoinTable, DS_DATASET_T *pTableOne,
	DS_DATASET_T *pTableTwo, char *aliases, char *joinList, char *projectList)
{
	char aliasName[2][DS_NAME_DIM];
	char *baseOne, *baseTwo, *joinBase, *names[2], *srcBase;
	size_t i, r1, r2, size, srcIndex[DS_MAX_JOIN];
	DS_BUF_T bp;	
	DS_FIELD_T *pJoinField, *srcField[DS_MAX_JOIN];
	DS_KEY_T key;
	DS_TYPE_T *types[4];

	if (!DS_IS_TABLE(pJoinTable) ||
		!DS_IS_TABLE(pTableOne)  ||
		!DS_IS_TABLE(pTableTwo)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	if (!dsTypePtr(&types[0], pJoinTable->tid) || 
		!dsTypePtr(&types[1], pTableOne->tid)  ||
		!dsTypePtr(&types[2], pTableTwo->tid)) {
		return FALSE;
	}
	types[3] = NULL;
	if (aliases == NULL) {
		names[0] = pTableOne->name;
		names[1] = pTableTwo->name;
	}
	else {
	 	DS_GET_INIT(&bp, aliases);
		names[0] = aliasName[0];
		names[1] = aliasName[1];
		if (dsGetName(names[0], &bp) <= 0 ||
			dsGetName(names[1], &bp) <= 0) {
			DS_ERROR(DS_E_INVALID_ALIAS);
		}       
   	}
	if (!dsEquijoinKey(&key, &types[1], names, joinList)) {
		return FALSE;
	}
	if (!dsEquijoinFields(srcField, srcIndex, types, &key, names, projectList)) {
		return FALSE;
	}
	pJoinField = DS_FIELD_PTR(types[0]);
	baseOne = pTableOne->p.data;
	pJoinTable->elcount = 0;
	for (r1 = 0; r1 < pTableOne->elcount; r1++) {
		baseTwo = pTableTwo->p.data;
		for (r2 = 0; r2 < pTableTwo->elcount; r2++) {
			if (dsCmpKeys(baseOne, baseTwo, &key) == 0) {
				if (pJoinTable->elcount >= pJoinTable->maxcount) {
					if (pJoinTable->elcount != pJoinTable->maxcount) {
						DS_ERROR(DS_E_SYSTEM_ERROR);
					}
					if (DS_IS_DYNAMIC(pJoinTable)) {
						if (!dsReallocTable(pJoinTable,
							DS_REALLOC_COUNT(pJoinTable))) {
							return FALSE;
						}
					}
					else {
						DS_ERROR(DS_E_JOIN_TOO_LARGE);
					}
				}
				joinBase = (char *)pJoinTable->p.data +
					pJoinTable->elcount *types[0]->size;
				for (i = 0; i < types[0]->nField; i++) {
					size = pJoinField[i].count*pJoinField[i].type->size;
					srcBase = srcIndex[i] ? baseTwo : baseOne;
					memcpy(joinBase + pJoinField[i].offset,
						srcBase + srcField[i]->offset, size);
				}
				pJoinTable->elcount++;
			}
			baseTwo += types[2]->size;
		}
		baseOne += types[1]->size;
	}
	if (DS_IS_DYNAMIC(pJoinTable)) {
		if (!dsReallocTable(pJoinTable, pJoinTable->elcount)) {
			return FALSE;
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsEquijoinField - find the source fields to be returned in the join
*
* RETURNS: TRUE if success else FALSE
*/
static int dsEquijoinFields(DS_FIELD_T **srcField, size_t *srcIndex,
	DS_TYPE_T **types, DS_KEY_T *key, char **names, char *projectList)
{
	size_t i, j, k, n;
	DS_FIELD_T *dstField, *field;

	if (!dsProjectFields(srcField, srcIndex, types[0],
		&types[1], names, projectList)) {
		return FALSE;
	}
	dstField = DS_FIELD_PTR(types[0]);
	n = types[0]->nField; 
	for (i = 0; i < n; i++) {
		if (srcField[i] != NULL) {
			continue;
		}
		for (j = 1; j < 3; j++) {
			if (dsFindField(&field, types[j], dstField[i].name) == 0) {
				if (srcField[i] != NULL) {
					for (k = 0; k < key->count; k++) {
						if (field == key->field[k][1]
							&& srcField[i] == key->field[k][0]) {
							break;
						}
					}
					if (k == key->count) {
						DS_ERROR(DS_E_AMBIGUOUS_JOIN);
					}
				}
				srcIndex[i] = j - 1;
				srcField[i] = field;
			}
		}
		if (srcField[i] == NULL) {
			DS_ERROR(DS_E_COLUMN_NOT_FOUND);
		}
		if (dsCmpFieldType(srcField[i], &dstField[i]) != 0) {
			DS_ERROR(DS_E_TYPE_MISSMATCH);
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsEquiJoinKey - determine the join fields
*
* RETURNS: TRUE if success else FALSE
*/
static int dsEquijoinKey(DS_KEY_T *key,
	DS_TYPE_T **types, char **names, char *joinList)
{
	char columnName[DS_NAME_DIM], tableName[DS_NAME_DIM];
	int c;
	size_t i, j, n;
	DS_BUF_T bp;
	DS_FIELD_T *f, *f0, *f1;
	
	if (joinList == NULL) {
		f1 = DS_FIELD_PTR(types[1]);
		n = types[1]->nField;
		for (key->count = i = 0; i < n; f1++, i++) {
			if ((c = dsFindField(&f0, types[0], f1->name)) != 0) {
				continue;
			}
			if (key->count >= DS_KEY_DIM) {
				DS_ERROR(DS_E_TOO_MANY_JOIN_FIELDS);
			} 
			key->field[key->count][0] = f0;
			key->field[key->count][1] = f1;
			key->count++;
		}
	}
	else { 
		DS_GET_INIT(&bp, joinList);
		if (dsGetNonSpace(&bp) != '{') {
			DS_ERROR(DS_E_INVALID_JOIN_SPECIFIER);
		}	
		for(key->count = 0;;) {
			if (key->count >= DS_KEY_DIM) {
				DS_ERROR(DS_E_TOO_MANY_JOIN_FIELDS);
			}
			key->field[key->count][0] = key->field[key->count][1] = NULL;
			for (i = 0; i < 2; i++) {
				if (!dsGetColumnSpecifier(tableName, columnName, &bp)) {
					DS_ERROR(DS_E_INVALID_JOIN_SPECIFIER);
				}
				for (j = 0; j < 2; j++) {
					if ((tableName[0] == '\0' ||
						strcmp(tableName, names[j]) == 0) &&
						dsFindField(&f, types[j], columnName) == 0) {
						if (key->field[key->count][j] != NULL) {
							DS_ERROR(DS_E_INVALID_JOIN_SPECIFIER);
						}
						key->field[key->count][j] = f;	
					}
				}
				if (!isalpha(c = dsGetNonSpace(&bp))) {
					break;
				}
				dsUngetc(c, &bp);
			}
			if (key->field[key->count][0] == NULL ||
				key->field[key->count][1] == NULL) {
				DS_ERROR(DS_E_INVALID_JOIN_SPECIFIER);
			}
			key->count++;
			if (c == '}') {
				break;
			}
			if (c != ',') {
				DS_ERROR(DS_E_INVALID_JOIN_SPECIFIER);
	 		}
	 	}
	} 
	if (key->count == 0) {
		DS_ERROR(DS_E_NO_JOIN_COLUMNS);
	}
	for (i = 0 ; i < key->count; i++) {
		if (dsCmpFieldType(key->field[i][0], key->field[i][1]) != 0) {
			DS_ERROR(DS_E_INVALID_JOIN_SPECIFIER);
		}
	}
	return TRUE;	 
}
/*****************************************************************************
*
* dsProjectFields - form list of source fields for project
*
* RETURNS: TRUE if success else FALSE
*/
static int dsProjectFields(DS_FIELD_T **srcField, size_t *srcIndex,
	DS_TYPE_T *dstType, DS_TYPE_T **srcType, char **names, char *projectList)
{
	char dstColumnName[DS_NAME_DIM];
	int c;
	size_t i, index, n;
	DS_BUF_T bp;
	DS_FIELD_T *dstField, *f, *field;

	dstField = DS_FIELD_PTR(dstType);
	n = dstType->nField; 
	if (n > DS_MAX_JOIN) {
		DS_ERROR(DS_E_TOO_MANY_PROJECT_FIELDS);
	}
	memset(srcField, 0, n*sizeof(srcField[0]));
	
	if (projectList == NULL) {
		return TRUE;
	}
	DS_GET_INIT(&bp, projectList);
	if (dsGetNonSpace(&bp) != '{') {
		DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
	}
	for (;;) {
		if (!dsTargetField(dstColumnName, &field,
			&index, srcType, names, &bp)) {
			return FALSE;
		}
		if (dsFindField(&f, dstType, dstColumnName) != 0) {
			DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
		}
		if (dsCmpFieldType(f, field) != 0) {
			DS_ERROR(DS_E_PROJECT_TYPE_MISSMATCH);
		}
		if ((i = f - dstField) >= DS_MAX_JOIN) {
			DS_ERROR(DS_E_SYSTEM_ERROR);
		}
		if (srcField[i] != NULL) {
			DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
		}
		srcField[i] = field;
		if (srcIndex != NULL) {
			srcIndex[i] = index;
		}			
		if ((c = dsGetNonSpace(&bp)) == '}') {
			break;
		}
		if (c != ',') {
			DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsProjectTable - project source table into desination table
*
* RETURNS: TRUE if success else FALSE
*/
int dsProjectTable(DS_DATASET_T *pDst, DS_DATASET_T *pSrc, char *projectList)
{
	char *dst, *names[2], *src;
	int c, colFound = 0;
	size_t dstStride, i, j, n, nRow, size, srcStride;
	enum {SUCCESS = 0, SMALL = 1, MISSING = 2, MISSMATCH = 4}status = SUCCESS;
	DS_FIELD_T *dstField, *field, *srcField[DS_MAX_JOIN];
	DS_TYPE_T *pDstType, *srcTypes[2];

	if (!DS_IS_TABLE(pDst) || !DS_IS_TABLE(pSrc)) {
		DS_ERROR(DS_E_INVALID_TABLE);
	}
	if (!dsTypePtr(&pDstType, pDst->tid) ||
		!dsTypePtr(&srcTypes[0], pSrc->tid)) {
		return FALSE;
	}
	if ((nRow = pSrc->elcount) > pDst->maxcount) {
		if (DS_IS_DYNAMIC(pDst)) {
			if (!dsReallocTable(pDst, nRow)) {
				return FALSE;
			}
		}
		else if (pDst->maxcount == 0) {
			DS_ERROR(DS_E_TABLE_FULL);
		}
		else {
			nRow = pDst->maxcount;
			DS_LOG_ERROR(DS_E_TABLE_TOO_SMALL);
			status |= SMALL;
		}
	}
	if (projectList == NULL && pDst->tid == pSrc->tid) {
		colFound = pDstType->nField;
		if (pSrc->p.data != pDst->p.data) {
			size = nRow*pDstType->size;
			memcpy(pDst->p.data, pSrc->p.data, size);
		}
	}
	else {
		names[0] = pSrc->name;
		names[1] = NULL;
		srcTypes[1] = NULL;
		if (!dsProjectFields(srcField, NULL, pDstType,
			srcTypes, names, projectList)) {
			return FALSE;
		}
		dstField =DS_FIELD_PTR(pDstType);
		n = pDstType->nField;
		for (i = 0; i < n; i++) {
			if (srcField[i] != NULL) {
				continue;
			}
			if ((c = dsFindField(&field,
				srcTypes[0], dstField[i].name)) != 0) {
				if (c < 0) {
					DS_ERROR(DS_E_NAMES_COLLIDE);
				}
				DS_LOG_ERROR(DS_E_MISSING_COLUMN);
				status |= MISSING;
			}
			else if (dsCmpFieldType(field, &dstField[i]) != 0) {
				DS_LOG_ERROR(DS_E_COLUMN_TYPE_MISSMATCH);
				status |= MISSMATCH;
			}
			else {
				srcField[i] = field;
			}
		}
		dstStride = pDstType->size;
		srcStride = srcTypes[0]->size;
		for (i = 0; i < n; i++) {
			size = dstField[i].count*dstField[i].type->size;
			dst = (char *)pDst->p.data + dstField[i].offset;
			if (srcField[i] != NULL) {
				src = (char *)pSrc->p.data + srcField[i]->offset;
				colFound++;
				for (j = nRow; j-- > 0; dst += dstStride, src += srcStride) {
					memcpy(dst, src, size);
				}
			}
			else {
				for (j = nRow; j-- > 0; dst += dstStride) {
					memset(dst, 0, size);
				}
			}
		}
	}
	if (!colFound) {
		DS_ERROR(DS_E_NO_COLUMNS_MATCH);
	}
	pDst->elcount = nRow;
	switch (status) {
		case SUCCESS:
			return TRUE;

		case MISSMATCH:
		case MISSING:
		case SMALL:
			return FALSE;

		default:
			DS_ERROR(DS_E_MULTIPLE_ERRORS);
	}
}
/*****************************************************************************
*
* dsTargetField - find type of target field
*
* RETURNS: TRUE if success else FALSE
*/
int dsTargetField(char *dstColumnName, DS_FIELD_T **ppSrcField,
	size_t *pSrcIndex, DS_TYPE_T **types, char **names, DS_BUF_T *bp)
{
	char srcColumnName[DS_NAME_DIM], srcTableName[DS_NAME_DIM];
	int c;
	size_t srcIndex, i;
	DS_FIELD_T *srcField, *f;

	if (!dsGetColumnSpecifier(srcTableName, srcColumnName, bp)) {
		DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
	}
	c = dsGetNonSpace(bp);
	dsUngetc(c, bp);
	if (isalpha(c)) {
		if (dsGetName(dstColumnName, bp) <= 0) {
			DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
		}
	}
	else {
		strcpy(dstColumnName, srcColumnName);
	}
	for (srcField = NULL, i = 0; i < 2 && types[i] != NULL; i++) {
		if ((srcTableName[0] == '\0' ||
			strcmp(names[i], srcTableName) == 0) &&
			dsFindField(&f, types[i], srcColumnName) == 0) {
			if (srcField != NULL) {
				DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
			}
			srcField= f;
			srcIndex = i;
		}
	}
	if (srcField == NULL) {
		DS_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
	}
	*ppSrcField = srcField;
	*pSrcIndex = srcIndex;
	return TRUE;
}
/*****************************************************************************
*
* dsTargetTable - construct target table for join or project
*
* RETURNS: TRUE if success else FALSE
*/
int dsTargetTable(DS_DATASET_T **ppTable, const char *tableName, const char *typeName, 
	DS_DATASET_T *parentOne, DS_DATASET_T *parentTwo, 
	char *aliases, char *projectList)
{
	char aliasNames[2][DS_NAME_DIM], *names[2];
	char typeSpecifier[DS_MAX_SPEC_LEN];  
	size_t i, tidList[3];
	DS_BUF_T bp;
	DS_DATASET_T *tables[3];

	tables[0] = parentOne;
	tables[1] = parentTwo;
	DS_GET_INIT(&bp, aliases);
	for (i = 0; i < 2 && tables[i] != NULL; i++) {
		if (!DS_IS_TABLE(tables[i])) {
			DS_ERROR(DS_E_INVALID_TABLE);
		}
		tidList[i] = tables[i]->tid; 
		if (aliases != NULL) {
			if (dsGetName(aliasNames[i], &bp) <= 0) { 
  		 		DS_ERROR(DS_E_INVALID_ALIAS);
  			}
  			names[i] = aliasNames[i];
		}
		else {
			names[i] = tables[i]->name;
		}
	}
	if (i == 0) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	tidList[i] = 0;
	if (!dsTargetTypeSpecifier(typeSpecifier, sizeof(typeSpecifier), 
		typeName, tidList, names, projectList)) {
		return FALSE;
	}
	return dsNewTable(ppTable, tableName, typeSpecifier, 0, NULL); 
}
/*****************************************************************************
*
* dsTargetTypeSpecifier - construct target type specifier
*
* RETURNS: TRUE if success else FALSE
*/
int dsTargetTypeSpecifier(char *str, size_t maxSize, const char *typeName, 
	size_t *tidList, char **names, char *projectList)
{
	char dstName[DS_NAME_DIM];
	int c;
	size_t i, index, j, n, size;
	DS_BUF_T bp;
	DS_FIELD_T *f, *field, *srcField;
	DS_TYPE_T *newType, *types[3];

	for (i = 0; i < 2 && tidList[i] != 0; i++) {
		if (!dsTypePtr(&types[i], tidList[i])) {
			return FALSE;
		}
	}
	types[i] = NULL;

	size = sizeof(DS_TYPE_T) + DS_MAX_JOIN*sizeof(DS_FIELD_T);
	if ((newType = dsTypeCalloc(size)) == NULL) {
		return FALSE;
	}
	field = DS_FIELD_PTR(newType);
	if (!dsCopyName(newType->name, typeName, NULL)) {
		DS_LOG_ERROR(DS_E_INVALID_TYPE_NAME);
		goto fail;
	}
	newType->code = DS_TYPE_STRUCT;	
	if (projectList != NULL) {
		if (names == NULL) {
			DS_ERROR(DS_E_NULL_POINTER_ERROR);
		}
		DS_GET_INIT(&bp, projectList);
		if (dsGetNonSpace(&bp) != '{') {
			DS_LOG_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
			goto fail;
		}
		for (;;) {
			if (!dsTargetField(dstName, &f, &index, types, names, &bp)) {
				goto fail;
			}
			if (( n = newType->nField) >= DS_MAX_JOIN) {
				DS_LOG_ERROR(DS_E_TOO_MANY_MEMBERS);
				goto fail;
			}
			memcpy(&field[n], f, sizeof(DS_FIELD_T));
			strcpy(field[n].name, dstName);
			newType->nField++;
			if ((c = dsGetNonSpace(&bp)) == '}') {
				break;
			}
			if (c != ',') {
				DS_LOG_ERROR(DS_E_INVALID_PROJECT_SPECIFIER);
				goto fail;
			}
		}
	}
	else {
		for (i = 0; types[i] != NULL; i++) {
			srcField = DS_FIELD_PTR(types[i]);
			for (j = 0; j < types[i]->nField; j++) {
				if (dsFindField(&f, newType, srcField[j].name) == 0) {
					if (dsCmpFieldType(f, &srcField[j]) != 0) {
						DS_LOG_ERROR(DS_E_TYPE_MISSMATCH);
						goto fail;
					}       
					continue;
				}
				if ((n = newType->nField) >= DS_MAX_JOIN) {
					DS_LOG_ERROR(DS_E_TOO_MANY_MEMBERS);
					goto fail;
				}
				memcpy(&field[n], &srcField[j], sizeof(DS_FIELD_T));
				newType->nField++;
			}
		}		
	}
	if (!dsFormatTypeSpecifier(str, maxSize, newType)) {
		goto fail;
	}
	dsTypeFree(newType, size);
	return TRUE;
fail:
	dsTypeFree(newType, size);
	return FALSE;
}
