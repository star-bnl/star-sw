/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dstype.h - include file for data structures */

/*
modification history
--------------------
01a,24apr93,whg  written.
*/
				
/*
DESCRIPTION
TBS ...
*/
#ifndef DSTYPE_H
#define DSTYPE_H

/******************************************************************************
*
* parameters and sizes
*
*/
#define DS_MAX_NEST	10	/* max levels of nested structs */
#define DS_MAX_DATASET	100	/* max number of datasets in dataset */
#define DS_MAX_DECL_LEN	1000	/* maximum length of declaration string */
#define DS_MAX_STRUCT	100	/* max number of struct defs in a type */
#define DS_MAX_TID	1000	/* max number of tids */
#define DS_TID_HASH_LEN	255	/* size of closed hash for tids (2^N -1) */
#define DS_MAX_DIMS	4	/* max number of dimensions for a field */
#define DS_NAME_DIM	32	/* size to hold longest name plus zero byte */
#define DS_MAX_NAME_SIZE (DS_NAME_DIM - 1)
/******************************************************************************
*
* enum for type codes
*
*/
typedef enum ds_code_t {
	DS_TYPE_CHAR = 0,	/* ascii character [0, 128) */
	DS_TYPE_BYTE, 		/* signed 8-bit integer (-128, 128) */
	DS_TYPE_U_BYTE,		/* unsigned 8-bit integer [0, 256) */
	DS_TYPE_SHORT,		/* signed 16-bit integer (-2^15, 2^15) */
	DS_TYPE_U_SHORT,	/* unsigned 16-bit integer [0, 2^16) */
	DS_TYPE_INT,		/* may be SHORT or LONG */
	DS_TYPE_U_INT,		/* may be U_SHORT or U_LONG */
	DS_TYPE_LONG,		/* signed 32-bit integer (-2^31, 2^31) */
	DS_TYPE_U_LONG,		/* unsigned 32-bit integer [0, 2^32) */
	DS_TYPE_FLOAT,		/* IEEE 32-bit floating point */
	DS_TYPE_DOUBLE,		/* IEEE 64-bit floating point */
	DS_TYPE_STRUCT,		/* only constructed type */
	DS_TYPE_COUNT		/* number of types */
}DS_CODE_T;

#define DS_BASIC_TYPE(type) (type->code < DS_TYPE_STRUCT)
#define DS_STD_REAL(type) ((type)->code < DS_TYPE_FLOAT ||\
		DS_IS_IEEE_FLOAT || (type)->code > DS_TYPE_DOUBLE)

#define DS_FIELD_PTR(type) ((DS_FIELD_T *)&(type)[1])
/******************************************************************************
*
* ds_type_t - node in type tree
*
*/
typedef struct ds_type_t {
	char name[DS_NAME_DIM];		/* type name */
	enum ds_code_t code;		/* type code */
	unsigned int flags;		/* type flags */
	size_t size;		/* in memory sizeof(type) */
	size_t modulus;		/* alignment modulus */
	size_t stdsize;		/* size in standard encoding */
	size_t stdmodulus;	/* modulus in standard encoding */
	size_t nField;		/* number of fields that follow */
}DS_TYPE_T;

/* type flags */
#define DS_STD_REP 0X1		/* type has standard encoding */
#define DS_REP_IS_STD(type) (((type)->flags & DS_STD_REP) != 0)
/******************************************************************************
*
* field in structure
*
*/
typedef  struct	ds_field_t {		/* if code is DS_TYPE_STRUCT */
	char name[DS_NAME_DIM];	/* field name */
	struct ds_type_t *type;	/* field element type */
	size_t count;	/* number of elements in field */
	size_t offset;	/* field offset from start of struct */
	size_t stdoffset;	/* field offset in standard encoding */
				/* array dimension or zero */
	size_t dim[DS_MAX_DIMS];
}DS_FIELD_T;
/******************************************************************************
*
* node in hiearchical data structure
*
*/
typedef struct ds_dataset_t {
	struct ds_dataset_t *parent;		/* parent of this node */
	char name[DS_NAME_DIM];			/* data structure name */
	unsigned int flags;			/* dataset flags */
	size_t tid;			/* tid of p.data or zero */
	size_t elcount;			/* number of children or rows */
	size_t maxcount;			/* max value for elcount */
	union {					/* if u != NULL */
		struct ds_dataset_t *child;	/* children if tid == zero */
		void *data;			/* table if tid != zero */
	}p;
}DS_DATASET_T;

/* DATASET flags */
#define DS_ALLOC_P	0X01		/* p.child or p.data is allocated */
#define DS_ALLOC_NODE	0X02		/* this node is allocated */

/* DATASET MACROS */
#define DS_IS_DATASET(pDataset) (pDataset->tid == 0 &&\
	pDataset->p.child != NULL)
#define DS_IS_TABLE(pDataset) (!DS_IS_DATASET(pDataset))

/* error code macro */
#define dsError dsErrorCode()
/******************************************************************************
*
* function prototypes
*
*/
int dsAddTable(DS_DATASET_T *pDataset, char *name,
	char *decl, size_t nRow, void *ppData);
int dsAllocTables(DS_DATASET_T *pDataset);
int dsFindDataset(DS_DATASET_T **ppChild, DS_DATASET_T *pDataset, char *name);
int dsFreeDataset(DS_DATASET_T *pDataset);
int dsMapTable(DS_DATASET_T *pDataset, char *name,
	char *decl, size_t *pCount, void *ppData);
int dsNewDataset(DS_DATASET_T **ppDataset, char *name, size_t setDim);
int dsPerror(char *msg);
int dsProjectTable(DS_DATASET_T *pDst, DS_DATASET_T *pSrc);
int dsTasProject(DS_DATASET_T *pDataset, char *name,
	char *decl, size_t *pCount, void *ppData);

/******************************************************************************
*
* Not recommended for applications
*
*/
#if defined(DS_ADVANCED) || defined(DS_PRIVATE)
int dsClearError(void);
int dsCmpFieldType(DS_FIELD_T *f1, DS_FIELD_T *f2);
int dsCreateDataset(DS_DATASET_T **pDataset,
	size_t dim, size_t *tList, char *str, char **ptr);
int dsCreateType(DS_TYPE_T **pType, size_t *pSize, char *str, char **ptr);
int dsErrorCode(void);
int dsFindField(DS_FIELD_T **pField, DS_TYPE_T *pType, char *name);
int dsFindTable(DS_DATASET_T *pDataset, char *name,
	char *decl, DS_DATASET_T **ppTable);
int dsFmtDatasetDef(char *str, size_t maxSize, DS_DATASET_T *pDataset);
int dsFmtTypeDef(char *str, size_t maxSize, DS_TYPE_T *type);
int dsPrintDataset(DS_DATASET_T *pDataset);
int dsTypeCmp(DS_TYPE_T *t1, DS_TYPE_T *t2);
int dsTypeDef(char **ptr, size_t *pLen, size_t tid);
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
#ifndef FALSE
#define FALSE 0
#endif
#ifndef NULL
#define NULL 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#ifdef DS_GLOBAL_ONE
float dsFloatOne = 1.0f;
long dsLongOne = 1;
#else
extern float dsFloatOne;
extern long dsLongOne;
#endif

/******************************************************************************
*
*
*/
int dsErrorLogger(int code, char *msg, char *file, int line);
#define DS_ERROR(code) {DS_LOG_ERROR(code); return FALSE;}
#define DS_LOG_ERROR(code) {dsErrorLogger(code, #code, __FILE__, __LINE__); }
#ifndef VXWORKS
#define DS_TRACE fprintf(stderr, "%s.%d\n", __FILE__, __LINE__)
#else
#define DS_TRACE printf("%s.%d\n", __FILE__, __LINE__)
#endif

#define DS_IEEEF_ONE	0X3F800000L
#define DS_VAXF_ONE	0X00004080L
#define DS_IS_BIG_ENDIAN	(((char *)&dsLongOne)[sizeof(dsLongOne)-1] == 1)
#define DS_IS_LITTLE_ENDIAN	(((char *)&dsLongOne)[0] == 1)
#define DS_IS_IEEE_FLOAT	(((long *)&dsFloatOne)[0] == DS_IEEEF_ONE)
#define DS_IS_VAX_FLOAT		(((long *)&dsFloatOne)[0] == DS_VAXF_ONE)


#define DS_MODULUS(x) (sizeof(struct tag{char c; x v;}) - sizeof(x))

#define DS_PAD(offset, modulus) (offset%modulus ? modulus - offset%modulus : 0)
/******************************************************************************
*
* descriptor for memory region
*
*/
typedef struct {
	char *first;	/* address of memory region */
	char *next;	/* next free byte of region */
	char *limit;	/* address of last byte of region plus one */
	int *map;
}DS_MEM_T;
/******************************************************************************
*
*/
int dsCheckDuplicate(char *name, size_t count, size_t stride);
int dsDatasetAllocStats(void);
char *dsDsetAlloc(size_t size);
void dsDsetFree(void *ptr);
int dsDumpTypes(void);
int dsIdentifier(char *dst, char *str, char **ptr);
int dsIndent(DS_MEM_T *bp, int level);
int dsIsName(char *str);
int dsMapDef(int *pSepCount, int *map, int *pMapCount,
	int maxMapCount, char *str);
int dsNameToMem(DS_MEM_T *mem, char *str);
int dsNCopyName(char *dst, char *str, char **ptr, int maxLen);
int dsNumToMem(DS_MEM_T *mem, size_t num);
int dsNextChar(char *str, char **ptr, int c);
int dsNextName(char *str, char **ptr, char *word);
int dsStrToMem(DS_MEM_T *bp, char *str);
char *dsTypeCalloc(size_t size);
void dsTidFree(void *ptr, size_t size);
int dsTidHashStats(void);
char *dsTypeLimit(DS_TYPE_T *type);
int dsTypeLock(void);
int dsTypeLockInit(void);
int dsTypeLockStats(void);
int dsTypeUnlock(void);
int dsTypeListCreate(size_t **pList, size_t listDim);
int dsTypeListFree(size_t *list);
int dsTypeListEnter(size_t *list, char *str, char **ptr);
int dsTypeListFind(size_t *pH, size_t *list, char *str, char **ptr);
/******************************************************************************
*
*
*/
#ifdef sun
#ifndef VXWORKS
#include <stdio.h>
int fprintf(FILE *stream, char *fmt, ...);
char *memset(char *ptr, int val, int len);
int printf(char *fmt, ...);
long strtol(char *str, char **ptr, int base);
#endif
#endif
#endif /* DS_PRIVATE */
#endif /* DSTYPE_H */
