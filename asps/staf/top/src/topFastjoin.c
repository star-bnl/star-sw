/* Copyright 1995, Lawrence Berkeley Laboratory */

/* dsjoin.c - routines that do joins and projects */

/*
modification history
--------------------
08aug97,hjw  join runs much faster for sorted tables (n instead of n squared)
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
#include "asuAlloc.h"
#include "emlLib.h"

static int topEquijoinFields(DS_FIELD_T **srcField, size_t *srcIndex,
	DS_TYPE_T **types, DS_KEY_T *key, char **names, char *projectList);
static int topEquijoinKey(DS_KEY_T *key,
	DS_TYPE_T **types, char **names, char *joinList);
static int topProjectFields(DS_FIELD_T **srcField, size_t *srcIndex,
	DS_TYPE_T *dstType, DS_TYPE_T **srcType, char **names, char *projectList);
/***********************************************************************
*
* topTablesNotSorted 
*
* RETURNS: non-zero if tables are not sorted, zero if they are sorted
*
*/
#define PLCOPY 100

/* Unused? 
   static char patchSL97a005[]="@(#)patchSL97a005.c topTablesNotSorted"; */

int topTablesNotSorted(DS_DATASET_T *tbl1,DS_DATASET_T *tbl2,
    char *projectList,
    float **vals1,float **vals2) {
  char *scratch,*col1,*col2,plCopy[PLCOPY+1];
  DS_DATASET_T *tbl;
  size_t colnum,col1num,col2num,nrow1,nrow2,nrow,row;
  DS_TYPE_CODE_T col1type,col2type,coltype;
  DS_PTR_UNION_T cellPointer;
  float *fArray,valForRow,valueFromPreviousRow=0; /* init is to prevent 
                                                  compiler warning */
  int ii,numSpaces=0,numCommas=0,whichTable;
  if(strlen(projectList)>PLCOPY) return __LINE__;
  strcpy(plCopy,projectList);
  for(ii=0;plCopy[ii];ii++) {
    if(plCopy[ii]==' ') numSpaces++; if(plCopy[ii]==',') numCommas++;
  }
  if(numCommas>0) return __LINE__;	/* can't sort by more than 1 col per table */
  if(numSpaces>1) return __LINE__;	/* syntax error in projectList */
  col1=strtok(plCopy,"{ }"); 
  if(!col1) return __LINE__;	/* syntax error in projectList */
  col2=strtok(NULL,"{ }"); 
  if(!col2) col1=col2;		/* col is the same for both tables */
  scratch=strstr(col1,"."); if(scratch) col1=scratch+1;
  scratch=strstr(col2,"."); if(scratch) col2=scratch+1;
  if(!dsTableRowCount(&nrow1,tbl1))			return __LINE__;
  if(!dsTableRowCount(&nrow2,tbl2))			return __LINE__;
  if(!dsFindColumn(&col1num,tbl1,col1))			return __LINE__;
  if(!dsFindColumn(&col2num,tbl2,col2))			return __LINE__;
  if(!dsColumnTypeCode(&col1type,tbl1,col1num))		return __LINE__;
  if(!dsColumnTypeCode(&col2type,tbl2,col2num))		return __LINE__;
  *vals1=(float*)MALLOC(nrow1*sizeof(float)); if(!*vals1) return __LINE__;
  *vals2=(float*)MALLOC(nrow2*sizeof(float)); if(!*vals2) { FREE(*vals1); return __LINE__; }
  for(whichTable=0;whichTable<2;whichTable++) {
    switch(whichTable) {
      case 0: coltype=col1type; nrow=nrow1; tbl=tbl1; colnum=col1num; fArray=*vals1; break;
      case 1: coltype=col2type; nrow=nrow2; tbl=tbl2; colnum=col2num; fArray=*vals2; break;
      default: return __LINE__;
    }
    for(row=0;row<nrow;row++) {      /* This loop runs twice, once for each table. */
      if(!dsCellAddress(&(scratch),tbl,row,colnum)) return __LINE__;
      cellPointer.v=scratch;
      switch(coltype) {
        case DS_TYPE_LONG:    valForRow=*(cellPointer.l ); break;
        case DS_TYPE_SHORT:   valForRow=*(cellPointer.s ); break;
        case DS_TYPE_FLOAT:   valForRow=*(cellPointer.f ); break;
        case DS_TYPE_DOUBLE:  valForRow=*(cellPointer.d ); break;
        case DS_TYPE_U_LONG:  valForRow=*(cellPointer.ul); break;
        case DS_TYPE_U_SHORT: valForRow=*(cellPointer.us); break;
        case DS_TYPE_STRUCT: case DS_TYPE_OCTET:
        case DS_TYPE_CHAR: default: return __LINE__;
      }
      if(row>0) { if(valueFromPreviousRow>valForRow) return __LINE__; }
      fArray[row]=valForRow;
      valueFromPreviousRow=valForRow;
    }
  }
  return 0;
}

#define DS_FREE_VALS if(sorted) { FREE(rValues1); FREE(rValues2); }

/***********************************************************************
*
* topFastjoin - rapidly construct projection of natural join of sorted tables
*
* RETURNS: TRUE if success else FALSE
*/
int topFastjoin(DS_DATASET_T *pJoinTable, DS_DATASET_T *pTableOne,
	DS_DATASET_T *pTableTwo, char *aliases, 
        char *joinList, char *projectList)
{
	char aliasName[2][DS_NAME_DIM];
	int startHere,sorted,notSorted;
	float *rValues1,*rValues2;
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
	if (!topEquijoinKey(&key, &types[1], names, joinList)) {
		return FALSE;
	}
	if (!topEquijoinFields(srcField, srcIndex, types, &key, names, projectList)) {
		return FALSE;
	}
	pJoinField = DS_FIELD_PTR(types[0]);
	baseOne = pTableOne->p.data;
	pJoinTable->elcount = 0;
	notSorted=topTablesNotSorted(pTableOne,pTableTwo,joinList,&rValues1,&rValues2);
	if(!notSorted) sorted=TRUE; else sorted=FALSE;
	if(!sorted) EML_ERROR(BOTH_TABLES_MUST_BE_SORTED);
	startHere=0;
	for (r1 = 0; r1 < pTableOne->elcount; r1++) {
		/* baseTwo = pTableTwo->p.data + (size_t)startHere*types[2]->size; */
		baseTwo = (char*)pTableTwo->p.data + startHere*types[2]->size;
		for (r2 = startHere; r2 < pTableTwo->elcount; r2++) {
			if(sorted) {
				if(r1<pTableOne->elcount-1) {
					if( rValues2[r2]<rValues1[r1+1] ) startHere=r2;
				}
				if(rValues2[r2]>rValues1[r1]) break;
			}
			if (dsCmpKeys(baseOne, baseTwo, &key) == 0) {
				if (pJoinTable->elcount >= pJoinTable->maxcount) {
					if (pJoinTable->elcount != pJoinTable->maxcount) {
						DS_ERROR(DS_E_SYSTEM_ERROR);
					}
					if (DS_IS_DYNAMIC(pJoinTable)) {
						if (!dsReallocTable(pJoinTable,
							DS_REALLOC_COUNT(pJoinTable))) {
							DS_FREE_VALS;
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
			DS_FREE_VALS;
			return FALSE;
		}
	}
	DS_FREE_VALS;
	return TRUE;
}

static int topEquijoinFields(DS_FIELD_T **srcField, size_t *srcIndex,
	DS_TYPE_T **types, DS_KEY_T *key, char **names, char *projectList)
{
	size_t i, j, k, n;
	DS_FIELD_T *dstField, *field;

	if (!topProjectFields(srcField, srcIndex, types[0],
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
static int topEquijoinKey(DS_KEY_T *key,
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
* topProjectFields - form list of source fields for project
*
* RETURNS: TRUE if success else FALSE
*/
static int topProjectFields(DS_FIELD_T **srcField, size_t *srcIndex,
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
