/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dstype.h - include file for data structures */

/*
modification history
--------------------
24apr93,whg  written.
23feb95,whg  CORBA style types
29may96,whg  ds_dataset_t struct with link indirection
21jul97,cet  add dsError
*/				
/*
DESCRIPTION
definitions for types and datasets
*/
#ifndef DSTYPE_H
#define DSTYPE_H 
#include <stdio.h>
/* rpc.h already defines TRUE, FALSE ... */
#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
#include <rpc/rpc.h>
#ifdef __cplusplus
extern "C" {
#endif
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
#define DS_ALLOC_FACTOR	1.5		/* realloc factor for dynamic tables */
#define DS_ALLOC_INC	100		/* alloc increment for dynamic datasets */
#define DS_BUF_ALLOC	500		/* allocation increment for string buffers */
#define DS_LIST_INC		100		/* number of items for list realloc */
#define DS_MAX_DIMS		4		/* max number of dimensions for a field */
#define DS_MAX_ERR		20		/* max number of thread error structures */
#define DS_MAX_JOIN 	100 	/* max number of fields in a join */
#define DS_MAX_NAME_LEN	31		/* max length of a name */
#define DS_MAX_NEST		10		/* max levels of nested structs */
#ifndef VXWORKS
#define DS_MAX_SPEC_LEN	100000	/* max length of specifier string */ /*Increased by VP*/
#else
#define DS_MAX_SPEC_LEN	1000	/* max length of specifier string */
#endif
#define DS_MAX_STRUCT	100		/* max number of struct defs in a type */
#define DS_MAX_TID		1000	/* max number of tids */
#define DS_NAME_DIM	(DS_MAX_NAME_LEN+1)/* size of name plus zero byte */ 
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
* union for basic types  
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
	size_t refcount;		/* refrence count of pointers to this node */
	size_t visit;			/* used in graph theory algorithms */
	union {						
		struct ds_dataset_t **link;	/* dataset if tid == zero */
		void *data;					/* table if tid != zero */
	}p;
}DS_DATASET_T;

/*
 * DATASET flags
 */
#define DS_F_ALLOC_P	0X01	/* p.link/p.data is allocated and not NULL*/
#define DS_F_ALLOC_NODE	0X02	/* this dataset struct is allocated */
#define DS_F_INVALID ((unsigned)~(DS_F_ALLOC_P|DS_F_ALLOC_NODE))
/*
 * DATASET flags macros
 */
#define DS_IS_VALID(pd) ((pd) != NULL &&\
	((pd)->flags & DS_F_INVALID) == 0 &&\
 	((pd)->p.data != NULL ? (pd)->elcount <= (pd)->maxcount:\
	(pd)->elcount == 0 && ((pd)->flags & DS_F_ALLOC_P) == 0))
#define DS_IS_DATASET(pd) (DS_IS_VALID(pd) && (pd)->tid == 0)
#define DS_IS_TABLE(pd)   (DS_IS_VALID(pd) && (pd)->tid != 0)
#define DS_IS_DYNAMIC(pd) ((pd)->p.data == NULL ||\
	((pd)->flags & DS_F_ALLOC_P) != 0)
#define DS_REALLOC_COUNT(x) ((size_t)(DS_ALLOC_FACTOR*((x)->maxcount + 1)))
/******************************************************************************
*
* function prototypes
*
*/
int dsAddTable(DS_DATASET_T *pDataset, const char *name,
	const char *typeSpecifier, size_t nRow, char **ppData);
int dsAllocTables(DS_DATASET_T *pDataset); 
int dsCellAddress(char **pAddress, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber); /* pAddress modif data*/
int dsColumnDimCount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnDimensions(size_t *dims, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnElcount(size_t *pCount, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnName(const char **pName, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnSize(size_t *pSize, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnTypeCode(DS_TYPE_CODE_T *pCode, DS_DATASET_T *pTable, size_t colNumber);
int dsColumnTypeName(const char **pName, DS_DATASET_T *pTable, size_t colNumber);
void dsAllocStats(void);
int dsDatasetEntry(DS_DATASET_T **ppEntry, DS_DATASET_T *pDataset,
	size_t entryNumber); 
int dsDatasetEntryCount(size_t *pCount, DS_DATASET_T *pDataset);
int dsDatasetMaxEntryCount(size_t *pCount, DS_DATASET_T *pDataset);  
int dsDatasetName(const char **pName, DS_DATASET_T *pDataset);
int dsEquijoin(DS_DATASET_T *pJoinTable,DS_DATASET_T *pTableOne,
	DS_DATASET_T *pTableTwo, char *aliases, char *joinLst, char *projectList);
int dsErrorCode(void);
int dsFindColumn(size_t *pColNumber, DS_DATASET_T *pTable, const char *name); 
int dsFindEntry(DS_DATASET_T **ppEntry, DS_DATASET_T *pDataset, const char *path);
int dsFreeDataset(DS_DATASET_T *pDataset);
int dsGetCell(char *address, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber); 
int dsInitTable(DS_DATASET_T *pTable, const char *tableName,
	const char *typeSpecifier, unsigned rowCount, void *pData);
int dsIsAcyclic(DS_DATASET_T *dataset);
int dsIsDataset(bool_t *pResult, DS_DATASET_T *handle);
int dsIsTable(bool_t *pResult, DS_DATASET_T *handle);
int dsLink(DS_DATASET_T *pParent, DS_DATASET_T *pChild);
int dsLinkAcyclic(DS_DATASET_T *pParent, DS_DATASET_T *pChild);
void dsLogError(DS_ERROR_CODE_T code, char *msg, char *file, size_t line);
int dsPutCell(const char *address, DS_DATASET_T *pTable,
	size_t rowNumber , size_t colNumber);
int dsMapTable(DS_DATASET_T *pDataset, const char *name,
	const char *typeSpecifier, size_t *pCount, char **ppData);
int dsNewDataset(DS_DATASET_T **ppDataset, const char *name);
int dsNewTable(DS_DATASET_T **ppTable, const char *tableName,
	const char *typeSpecifier,  unsigned rowCount, void *pData);
void dsPerror(const char *msg);
const char * dsError(const char *msg);
int dsProjectTable(DS_DATASET_T *pDst, DS_DATASET_T *pSrc, char *projectList);
int dsRealloc(DS_DATASET_T *dataset, size_t maxcount);
int dsReallocTable(DS_DATASET_T *pTable, size_t nRow);
int dsRefcount(size_t *pCount, DS_DATASET_T *pDataset);
int dsSetTableRowCount(DS_DATASET_T *pTable, size_t rowCount);
int dsTableColumnCount(size_t *pCount, DS_DATASET_T *pTable);
int dsTableDataAddress(char **pAddress, DS_DATASET_T *pTable);
int dsTableIsType(bool_t *pResult, DS_DATASET_T *pTable, const char *specifier);
int dsTableMaxRowCount(size_t *pCount, DS_DATASET_T *pTable);
int dsTableName(const char **pName, DS_DATASET_T *pTable);
int dsTableRowCount(size_t *pRowCount, DS_DATASET_T *pTable);
int dsTableRowSize(size_t *pSize, DS_DATASET_T *pTable);
int dsTableTypeName(const char **pName, DS_DATASET_T *pTable);
int dsTableTypeSpecifier(const char **pSpecifier, DS_DATASET_T *pTable);
int dsTargetTable(DS_DATASET_T **ppTable, const char *tableName, const char *typeName, 
	DS_DATASET_T *parentOne, DS_DATASET_T *parentTwo, 
	char *aliases, char *projectList);
int dsTasProject(DS_DATASET_T *pDataset, const char *name,
	const char *typeSpecifier, size_t *pCount, void *ppData);
 int dsUnlink(DS_DATASET_T *pParent, DS_DATASET_T *pChild);
/******************************************************************************
*
* Not recommended for applications
*
*/
/* Making these decls public since tbr uses them 
   #if defined(DS_ADVANCED) || defined(DS_PRIVATE) */
#if 1
/*****************************************************************************
*
* ds_list_t - structure for list of tables or datasets
*
*/
typedef struct ds_list_t {
	size_t count;			/* number of items */
	size_t maxcount;		/* max value for count */
	DS_DATASET_T **pItem;	/* list items */
}DS_LIST_T;
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
int dsCreateDataset(DS_DATASET_T **ppDataset,
					 size_t *tList, const char *str, const char **ptr);
int dsErrorPrint(char *fmt, ...);
int dsFindField(DS_FIELD_T **pFound, DS_TYPE_T *pType, const char *name);
int dsFindTable(DS_DATASET_T **ppTable, DS_DATASET_T *pDataset, const char *name,
    const char *typeSpecifier);
int dsFormatTypeSpecifier(char *str, size_t maxSize, DS_TYPE_T *type);
int dsListAppend(DS_LIST_T *list, DS_DATASET_T *item);
int dsListFree(DS_LIST_T *list);
int dsListInit(DS_LIST_T *list);
int dsListRealloc(DS_LIST_T *list, size_t maxcount);
int dsMark(DS_LIST_T *list, DS_DATASET_T *item);
int dsParseType(DS_TYPE_T **pType, size_t *pSize, const char *str, const char **ptr);
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
int dsTargetTypeSpecifier(char *str, size_t maxSize, const char *typeName, 
	size_t *tidList, char **names, char *projectList);
int dsTypeCmp(DS_TYPE_T *t1, DS_TYPE_T *t2);
int dsTypeSpecifier(const char **ptr, size_t *pLen, size_t tid);
int dsTypeId(size_t *pTid, const char *str, const char **ptr);
int dsTypePtr(DS_TYPE_T **pType, size_t tid);
int dsVisited(DS_LIST_T *list, DS_DATASET_T *item);
int dsVisitClear(DS_DATASET_T *dataset);
int dsVisitCount(DS_DATASET_T *dataset);
int dsVisitList(DS_LIST_T *list, DS_DATASET_T *dataset);
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
#define DS_GET_INIT(bp, str) {(bp)->first = (bp)->out = (char*)str;\
		(bp)->in = (bp)->limit = NULL;}
#define DS_GETC(bp) (((bp)->in == NULL && *(bp)->out) || ((bp)->in &&\
		(bp)->in > (bp)->out) ? 0XFF & (char)*(bp)->out++ : EOF)
#define DS_PEEK(bp) (((bp)->in == NULL && *(bp)->out) || ((bp)->in &&\
		(bp)->in > (bp)->out) ? 0XFF & (char)*(bp)->out : EOF)
#define DS_PUT_INIT(bp, buf, size) {(bp)->limit = (buf) + ((buf) ? (size) : 0);\
		(bp)->first = (bp)->in = (bp)->out = (buf);}		
#define DS_PUTC(c, bp) ((bp)->in < (bp)->limit ?\
		0XFF & (*(bp)->in++ = (char)(c)) : dsPutc(c, bp))
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
* private functions
*
*/
int dsBufFree(DS_BUF_T *bp);
int dsBufRealloc(DS_BUF_T *bp, size_t size);
int dsCmpKeys(char *baseOne, char *baseTwo, DS_KEY_T *key);
int dsCmpName(const char *s1, const char *s2);
int dsCopyName(char *dst, const char *str, const char **ptr);
int dsDatasetSpecifier(DS_BUF_T *bp, DS_DATASET_T *pDataset);
int dsDumpTypes(void);
int dsErrSemGive(void);
int dsErrSemTake(void);
int dsGetc(DS_BUF_T *mp);
int dsGetColumnSpecifier(char *tableName, char *columnName, DS_BUF_T *bp);
int dsGetName(char *name, DS_BUF_T *bp);
int dsGetNonSpace(DS_BUF_T *mp);
int dsGetNumber(size_t *pNumber, DS_BUF_T *bp);
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
int dsTypeListEnter(size_t *list, const char *str, const char **ptr);
int dsTypeListFind(size_t *pH, size_t *list, char *str);
int dsTypeListFree(size_t *list);
int dsTypeSemGive(void);
int dsTypeSemTake(void);
int dsUngetc(int c, DS_BUF_T *bp);
#endif  /* DS_PRIVATE */
#ifdef __cplusplus
}
#endif
#endif  /* DSTYPE_H */
