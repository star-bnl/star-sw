/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dstype.h - include file for data structures */

/*
modification history
--------------------
24apr93,whg  written.
23feb95,whg  CORBA style types
*/				
/*
DESCRIPTION
definitions for types and datasets
*/
#ifndef DSTYPE_H
#define DSTYPE_H 
#include <stdio.h>
#include <rpc/rpc.h>
#include "dscodes.h"
/******************************************************************************
*
* macros for return values and types
*
*/
#ifndef FALSE
#define FALSE 0
#endif
#ifndef NULL
#define NULL 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
/******************************************************************************
*
* parameters and sizes
*
*/
#define DS_MAX_DATASET	100		/* max number of datasets in dataset */
#define DS_MAX_DIMS		4		/* max number of dimensions for a field */
#define DS_MAX_JOIN 	100 	/* max number of fields in a join */
#define DS_MAX_NAME_LEN	31		/* max length of a name */
#define DS_MAX_NEST		10		/* max levels of nested structs */
#define DS_MAX_SPEC_LEN	1000	/* maximum length of specifier string */
#define DS_MAX_STRUCT	100		/* max number of struct defs in a type */
#define DS_MAX_ERR		20		/* max number of thread error structures */
#define DS_MAX_TID		1000	/* max number of tids */
#define DS_NAME_DIM	(DS_MAX_NAME_LEN+1)/* size of name plus zero byte */ 
#define DS_ALLOC_FACTOR	1.5		/* realloc factor for dynamic tables */
#define DS_TID_HASH_LEN	255		/* size of tid closed hash (2^N -1) */
/******************************************************************************
*
* enum for type codes
*
*/
typedef enum ds_type_code_t {
	DS_TYPE_CHAR = 0,	/* ascii character [0, 128) */
	DS_TYPE_OCTET,		/* unsigned 8-bit integer [0, 256) */
	DS_TYPE_SHORT,		/* signed 16-bit integer (-2^15, 2^15) */
	DS_TYPE_U_SHORT,	/* unsigned 16-bit integer [0, 2^16) */
	DS_TYPE_LONG,		/* signed 32-bit integer (-2^31, 2^31) */
	DS_TYPE_U_LONG,		/* unsigned 32-bit integer [0, 2^32) */
	DS_TYPE_FLOAT,		/* IEEE 32-bit floating point */
	DS_TYPE_DOUBLE,		/* IEEE 64-bit floating point */
	DS_TYPE_STRUCT		/* only constructed type */
}DS_TYPE_CODE_T;
/******************************************************************************
*
* type defs for basic types  
*/
typedef unsigned char octet;	/* CORBA transparent eight bit type */

typedef union ds_ptr_union_t {	/* union of pointers to basic types */
	char *c;
	octet *o;
	short *s;
	unsigned short *us;
	long *l;
	unsigned long *ul;
	float *f;
	double *d;
	void *v;
}DS_PTR_UNION_T;
/******************************************************************************
*
* node in hiearchical data structure
*
*/
typedef struct ds_dataset_t {
	char name[DS_NAME_DIM];	/* dataset name */
	unsigned int flags;		/* dataset flags */
	size_t tid;				/* typeID of p.data or zero */
	size_t elcount;			/* number of children or rows */
	size_t maxcount;		/* max value for elcount */
	union {						
		struct ds_dataset_t *child;	/* children if tid == zero */
		void *data;					/* table if tid != zero */
	}p;
}DS_DATASET_T;
/*
 * macros to check for dataset or table
 * they return FALSE for invalid structs
 */
#define DS_IS_DATASET(pd) (dsValidDataset(pd) && (pd)->tid == 0)
#define DS_IS_TABLE(pd)   (dsValidDataset(pd) && (pd)->tid != 0)
/*
 * DATASET flags and macros for dynamic memory
 */
#define DS_ALLOC_P		0X01	/* p.child or p.data is allocated */
#define DS_ALLOC_NODE	0X02	/* this node is allocated */
#define DS_DATASET_FLAGS (DS_ALLOC_P | DS_ALLOC_NODE)

#define DS_DYNAMIC_TABLE(pd) ((pd)->p.data == NULL ||\
	((pd)->flags & DS_ALLOC_P) != 0)
#define DS_REALLOC_COUNT(x) ((size_t)(DS_ALLOC_FACTOR*((x)->maxcount + 1)))
/******************************************************************************
*
* function prototypes
*
*/
int dsAddDataset(DS_DATASET_T *pDataset, char *dsetName,
	size_t setDim, DS_DATASET_T *pChild);
int dsAddTable(DS_DATASET_T *pDataset, char *name,
	char *typeSpecifier, size_t nRow, char **ppData);
int dsAllocTables(DS_DATASET_T *pDataset); 
int dsCellAddress(char **pAddress, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber);
int dsColumnDimCount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnDimensions(size_t *dims, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnElcount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnName(char **pName, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnSize(size_t *pSize, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnTypeCode(DS_TYPE_CODE_T *pCode, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnTypeName(char **pName, DS_DATASET_T *pTable, size_t colNumber);
void dsDatasetAllocStats(void);
int dsDatasetEntry(DS_DATASET_T **ppEntry, DS_DATASET_T *pDataset,
	size_t entryNumber); 
int dsDatasetEntryCount(size_t *pCount, DS_DATASET_T *pDataset);
int dsDatasetMaxEntryCount(size_t *pCount, DS_DATASET_T *pDataset);  
int dsDatasetName(char **pName, DS_DATASET_T *pDataset);
int dsDeleteEntry(DS_DATASET_T *pEntry, DS_DATASET_T *pDataset);
int dsEquijoin(DS_DATASET_T *pJoinTable,DS_DATASET_T *pTableOne,
	DS_DATASET_T *pTableTwo, char *aliases, char *joinLst, char *projectList);
int dsErrorCode(void);
int dsFindColumn(size_t *pColNumber, DS_DATASET_T *pTable, char *name); 
int dsFindEntry(DS_DATASET_T **ppEntry, DS_DATASET_T *pDataset, char *path);
int dsFreeDataset(DS_DATASET_T *pDataset);
int dsGetCell(char *address, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber); 
int dsIsDataset(bool_t *pResult, DS_DATASET_T *handle);
int dsIsTable(bool_t *pResult, DS_DATASET_T *handle);
void dsLogError(DS_ERROR_CODE_T code, char *msg, char *file, size_t line);
int dsPutCell(char *address, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber);
int dsMapTable(DS_DATASET_T *pDataset, char *name,
	char *typeSpecifier, size_t *pCount, char **ppData);
int dsNewDataset(DS_DATASET_T **ppDataset, char *name, size_t setDim);
int dsNewTable(DS_DATASET_T **ppTable, char *tableName,
	char *typeSpecifier,  unsigned rowCount, void *pData);
void dsPerror(char *msg);
int dsProjectTable(DS_DATASET_T *pDst, DS_DATASET_T *pSrc, char *projectList);
int dsReallocTable(DS_DATASET_T *pTable, size_t nRow);
int dsSetTableRowCount(DS_DATASET_T *pTable, size_t rowCount);
int dsTableColumnCount(size_t *pCount, DS_DATASET_T *pTable);
int dsTableDataAddress(char **pAddress, DS_DATASET_T *pTable);
int dsTableIsType(bool_t *pResult, DS_DATASET_T *pTable, char *specifier);
int dsTableMaxRowCount(size_t *pCount, DS_DATASET_T *pTable);
int dsTableName(char **pName, DS_DATASET_T *pTable);
int dsTableRowCount(size_t *pRowCount, DS_DATASET_T *pTable);
int dsTableRowSize(size_t *pSize, DS_DATASET_T *pTable);
int dsTableTypeName(char **pName, DS_DATASET_T *pTable);
int dsTableTypeSpecifier(char **pSpecifier, DS_DATASET_T *pTable);
int dsTargetTable(DS_DATASET_T **ppTable, char *tableName, char *typeName, 
	DS_DATASET_T *parentOne, DS_DATASET_T *parentTwo, 
	char *aliases, char *projectList);
int dsTasProject(DS_DATASET_T *pDataset, char *name,
	char *typeSpecifier, size_t *pCount, void *ppData);
/******************************************************************************
*
* Not recommended for applications
*
*/
#if defined(DS_ADVANCED) || defined(DS_PRIVATE)
/******************************************************************************
*
* ds_type_t - type node in parse tree
*
*/
typedef struct ds_type_t {
	char name[DS_NAME_DIM];	/* type name */
	DS_TYPE_CODE_T code;	/* type code */
	unsigned int flags;		/* type flags */
	size_t size;			/* in memory sizeof(type) */
	size_t modulus;			/* alignment modulus */
	size_t stdsize;			/* size in standard encoding */
	size_t stdmodulus;		/* modulus in standard encoding */
	size_t nField;			/* number of fields that follow */
}DS_TYPE_T;
/* 
 * macros to classify types
 */
#define DS_BASIC_TYPE(type) (type->code < DS_TYPE_STRUCT)
#define DS_IS_REAL(type) ((type)->code == DS_TYPE_FLOAT ||\
	(type)->code == DS_TYPE_DOUBLE)
/*
 * macros to initialize basic types
 */
#define DS_MODULUS_STRUCT(s, c, t) struct c ## _MOD{char x; t y;}c ## _MOD_T

#define DS_TYPE_INIT(s, c, t) {#t, c, 0, sizeof(t),\
	sizeof(c ## _MOD_T) - sizeof(t), s, s, 0}

/* type flags */
#define DS_NOT_STD_REP 0X1	/* type does not have standard encoding */
#define DS_REP_IS_STD(type) (((type)->flags & DS_NOT_STD_REP) == 0)
/******************************************************************************
*
* ds_field_t - field node for structs in parse tree
*
*/
typedef  struct	ds_field_t {
	char name[DS_NAME_DIM];		/* field name */
	struct ds_type_t *type;		/* field element type */
	size_t count;				/* number of elements in field */
	size_t offset;				/* field offset from start of struct */
	size_t stdoffset;			/* field offset in standard encoding */								
	size_t dim[DS_MAX_DIMS];	/* array dimension or zero */
}DS_FIELD_T;

/* fields follow type struct */
#define DS_FIELD_PTR(type) ((DS_FIELD_T *)&(type)[1])
/*
 * function for test and internal use - not in defined API
 */       
int dsBinSearch(char **pFound, char *value, char *base, size_t count,
	size_t size, int (*cmp)(char *base1, char *base2, char *key), char *key);
int dsCheckDataset(DS_DATASET_T *pSet);
int dsCheckTable(void *pData, char *decl, size_t nRow, size_t checkNRow);
int dsCmpFieldType(DS_FIELD_T *f1, DS_FIELD_T *f2);
int dsColumnField(DS_FIELD_T **ppField, DS_DATASET_T *pTable, size_t colNumber);
int dsCreateDataset(DS_DATASET_T **pDataset,
	size_t dim, size_t *tList, char *str, char **ptr);
int dsErrorPrint(char *fmt, ...);
int dsFindField(DS_FIELD_T **pFound, DS_TYPE_T *pType, char *name);
int dsFindTable(DS_DATASET_T **ppTable, DS_DATASET_T *pDataset, char *name,
	char *typeSpecifier);
int dsDatasetSpecifier(char *str, size_t maxSize, DS_DATASET_T *pDataset);
int dsFormatTypeSpecifier(char *str, size_t maxSize, DS_TYPE_T *type);
int dsParseType(DS_TYPE_T **pType, size_t *pSize, char *str, char **ptr);
void dsPrintData(FILE *stream, DS_TYPE_T *type, unsigned count, void *data);
int dsPrintDatasetSpecifier(FILE *stream, DS_DATASET_T *pDataset);
int dsPrintSpecifiers(FILE *stream, DS_DATASET_T *pDataset);
void dsPrintTableData(FILE *stream, DS_DATASET_T *table);
void dsPrintTableType(FILE *stream, DS_DATASET_T *pTable);
int dsPrintTypes(FILE *stream, DS_DATASET_T *pDataset, size_t *tList);
int dsQuickSort(char *base, unsigned count, int size,
	int (*cmp)(char * base1, char *base2, char *key), char *key);
int dsSetDataset(DS_DATASET_T *pSet);
int dsSetTable(void *pData, char *decl, size_t elcount);
int dsTableType(DS_TYPE_T **ppType, DS_DATASET_T *pTable);
int dsTargetTypeSpecifier(char *str, size_t maxSize, char *typeName, 
	size_t *tidList, char **names, char *projectList);
int dsTypeCmp(DS_TYPE_T *t1, DS_TYPE_T *t2);
int dsTypeSpecifier(char **ptr, size_t *pLen, size_t tid);
int dsTypeId(size_t *pTid, char *str, char **ptr);
int dsTypePtr(DS_TYPE_T **pType, size_t tid);
#endif /* DS_ADVANCED */

/******************************************************************************
*
* private definitions for ds library - not to be used in applications
*
*/
#ifdef DS_PRIVATE
/******************************************************************************
*
* private macros and variables
*
*/
#ifdef DS_GLOBAL_ONE
long dsLongOne = 1;			/* used to determine byte addressing */
float dsFloatOne = 1.0f;	/* used to determine floating point rep */
#else
extern long dsLongOne;
extern float dsFloatOne;
#endif
#define DS_ERROR(code) {DS_LOG_ERROR(code); return FALSE;}
#define DS_LOG_ERROR(code) {dsLogError(code, #code, __FILE__, __LINE__); }
#define DS_TRACE dsErrorPrint("%s.%d\n", __FILE__, __LINE__)

#define DS_IEEEF_ONE		0X3F800000L
#define DS_VAXF_ONE			0X00004080L
#define DS_IS_BIG_ENDIAN	(((char *)&dsLongOne)[sizeof(dsLongOne)-1] == 1)
#define DS_IS_LITTLE_ENDIAN	(((char *)&dsLongOne)[0] == 1)
#define DS_IS_IEEE_FLOAT	(((long *)&dsFloatOne)[0] == DS_IEEEF_ONE)
#define DS_IS_VAX_FLOAT		(((long *)&dsFloatOne)[0] == DS_VAXF_ONE)

#define DS_PAD(offset, modulus) (offset%modulus ? modulus - offset%modulus : 0)
/******************************************************************************
*
* descriptor for buffer
*
*/
typedef struct ds_buf_t {
	char *first;	/* address of first location of buffer */
	char *in;		/* address to put next byte in buffer */
	char *out;		/* address of get next byte from buffer */
	char *limit;	/* address of byte just after buffer */ 
}DS_BUF_T;
/*
 * macros get and put characters
 */
#define DS_GET_INIT(bp, str) {(bp)->first = (bp)->out = str;\
		(bp)->in = (bp)->limit = NULL;}
#define DS_GETC(bp) (((bp)->in == NULL && *(bp)->out) || ((bp)->in &&\
		(bp)->in > (bp)->out) ? 0XFF & (char)*(bp)->out++ : EOF)

#define DS_PEEK(bp) (((bp)->in == NULL && *(bp)->out) || ((bp)->in &&\
		(bp)->in > (bp)->out) ? 0XFF & (char)*(bp)->out : EOF)
#define DS_PUT_INIT(bp, buf, size) {(bp)->limit = (buf) + (size);\
		(bp)->first = (bp)->in = (bp)->out = (buf);}		
#define DS_PUTC(c, bp) ((bp)->in < (bp)->limit ?\
		0XFF & (*(bp)->in++ = (char)(c)) : EOF)

/******************************************************************************
*
* descriptor for table key
*
*/
#define DS_KEY_DIM 10
typedef struct ds_key_t {
	size_t count;
	DS_FIELD_T *field[DS_KEY_DIM][2];
}DS_KEY_T;
/******************************************************************************
*
* descriptor for memory region
*
*/
typedef struct {
	char *first;	/* address of memory region */
	char *next;		/* next free byte of region */
	char *limit;	/* address of last byte of region plus one */
	int *map;
}DS_MEM_T;
/******************************************************************************
*
* private functions
*
*/
int dsCheckDuplicate(char *name, size_t count, size_t stride);
int dsCmpKeys(char *baseOne, char *baseTwo, DS_KEY_T *key);
int dsCmpName(char *s1, char *s2);
int dsCopyName(char *dst, char *str, char **ptr);
void *dsDsetAlloc(size_t size);
void dsDsetFree(void *ptr);
void *dsDsetRealloc(char *old, size_t size);
int dsDumpTypes(void);
int dsErrSemGive(void);
int dsErrSemTake(void);
int dsFirstPass(int *pSepCount, int *map, int *pMapCount,
	int maxMapCount, char *str);
int dsGetc(DS_BUF_T *mp);
int dsGetColumnSpecifier(char *tableName, char *columnName, DS_BUF_T *bp);
int dsGetName(char *name, DS_BUF_T *bp);
int dsGetNonSpace(DS_BUF_T *mp);
int dsGetNumber(unsigned *pNumber, DS_BUF_T *bp);
int dsPutc(int c, DS_BUF_T *bp);
int dsPutNumber(int n, DS_BUF_T *bp);
int dsPuts(char *s, DS_BUF_T *bp);
int dsPutTabs(int n, DS_BUF_T *bp);
int dsSemInit(void);
int dsSemStats(void);
int dsTargetField(char *dstColumnName, DS_FIELD_T **ppSrcField,
	size_t *pSrcIndex, DS_TYPE_T **types, char **names, DS_BUF_T *bp);
void dsToDo(void);
void *dsTypeCalloc(size_t size);
void dsTypeFree(void *ptr, size_t size);
int dsTidHashStats(void);
char *dsTypeLimit(DS_TYPE_T *type);
int dsTypeListCreate(size_t **pList, size_t listDim);
int dsTypeListEnter(size_t *list, char *str, char **ptr);
int dsTypeListFind(size_t *pH, size_t *list, char *str);
int dsTypeListFree(size_t *list);
int dsTypeSemGive(void);
int dsTypeSemTake(void);
int dsUngetc(int c, DS_BUF_T *bp);
int dsValidDataset(DS_DATASET_T *pDset);
/******************************************************************************
*
* ANSI stuff for sun
*/
/*
#ifdef sun
#ifndef VXWORKS
#include <stdio.h>
int fprintf(FILE *stream, char *fmt, ...);
char *memset(char *ptr, int val, int len);
int printf(char *fmt, ...);
long strtol(char *str, char **ptr, int base);
#endif
#endif
*/
#endif  /* DS_PRIVATE */
#endif  /* DSTYPE_H */
