/*- ahslib.h -*/
#ifndef AHSLIB_H
#define AHSLIB_H

#ifndef NULL
#define NULL 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#ifndef SHAPE_T
typedef struct shape_t {
	int	rank;	/* number of indexes */
	int*	indexes;/* array of indexes */
} SHAPE_T;
#endif

typedef struct ahsNode_t {
	char*	name;	/* path name */
	SHAPE_T	shape;	/* path shape */
} AHSNODE_T;

typedef struct ahs_struct_t {
	char*	location;	/* root location -or- NULL */
	int	pathDepth;	/* path depth (>=0) */
	AHSNODE_T* nodes;	/* path arrays */
	char*	qualifier;	/* block name -or- NULL */
	char*	entryName;	/* column name -or- NULL */
	SHAPE_T	entryShape;	/* column indexes (>=0) */
} AHS_STRUCT_T;

#ifdef __cplusplus
extern "C" int ahs_parseSpec(char* s, AHS_STRUCT_T *p);
extern int ahs_zeroAHS(AHS_STRUCT_T& a);
extern "C" int ahs_printAHS(AHS_STRUCT_T p);
extern int ahs_specSplit(const char *spec
		, char*& loc, char*& path, char*& qual, char*& entr);
extern int ahs_spec2nodes(const char *spec, AHSNODE_T*& nodes);
extern int ahs_spec2nameShape(const char *spec, char*& name
		, SHAPE_T& shape);
extern int ahs_spec2shape(const char *spec, SHAPE_T& shape);
extern "C" int isValidAhsSpec(char *spec);
extern "C" int isValidShapeSpec(char *spec);
extern "C" int strsplit(const char * str,const char * del,char*** a);
extern "C" int isInteger(char* c);
#else /*__cplusplus*/
extern int ahs_parseSpec(char* s, AHS_STRUCT_T *p);
extern int ahs_printAHS(AHS_STRUCT_T p);
extern int isValidAhsSpec(char *spec);
extern int isValidShapeSpec(char *spec);
extern int strsplit(const char * str,const char * del,char*** a);
extern int isInteger(char* c);
#endif /*__cplusplus*/

#endif /*AHSLIB_H*/
